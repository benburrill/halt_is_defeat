from . import asm
from . import stdlib
from hidc import ast
from hidc.ast import DataType, ArrayType, ExitMode
from .symbols import ConcreteType, ConcreteArrayType, ConcreteSignature, AccessMode
from hidc.errors import CodeGenError, InternalCompilerError

from collections import deque, ChainMap
import dataclasses as dc
import contextlib
import typing as ty


@dc.dataclass(frozen=True)
class ArrayRef:
    type: ConcreteArrayType
    origin: asm.Accessor
    length: asm.Accessor

    @property
    def section(self):
        return self.type.access.section

    def make_const(self):
        if self.type.access == AccessMode.RW:
            return ArrayRef(
                ConcreteArrayType(self.type.el_type, AccessMode.R),
                origin=self.origin, length=self.length
            )
        return self


@dc.dataclass(frozen=True)
class StackPoint:
    offset: int = 0
    array_num: int = 0
    static_array_size: int = 0

    def add(self, *, offset=0, array_num=0, static_array_size=0):
        return StackPoint(
            self.offset + offset,
            self.array_num + array_num,
            self.static_array_size + static_array_size
        )

    def __bool__(self):
        return bool(self.offset or self.array_num or self.static_array_size)


@dc.dataclass(frozen=True)
class Bubble:
    prev: StackPoint
    cur: StackPoint

    def __add__(self, other):
        # Adjacent Bubbles can be combined together, but ValueBubbles
        # lose their contents
        if isinstance(other, Bubble):
            if self.cur == other.prev:
                return Bubble(self.prev, other.cur)
            raise ValueError('Bubbles are non-adjacent')
        return NotImplemented

    @property
    def frame_size(self):
        frame_size = self.cur.offset - self.prev.offset
        assert frame_size >= 0
        # If there's no change in offset, there should be nothing else
        if frame_size == 0: assert self.vacuous
        return frame_size

    @property
    def static_array_size(self):
        size = self.cur.static_array_size - self.prev.static_array_size
        assert size >= 0
        # Static array size implies there was an array allocation
        if size != 0: assert self.array_allocations >= 1
        return size

    @property
    def array_allocations(self):
        array_allocations = self.cur.array_num - self.prev.array_num
        assert array_allocations >= 0
        # New array allocations should have corresponding reference
        if array_allocations > 0: assert self.frame_size > 0
        return array_allocations

    @property
    def has_array(self):
        return self.array_allocations > 0 or self.static_array_size > 0

    @property
    def vacuous(self):
        return self.prev == self.cur

    def with_value(self, value, fast_value=None):
        return ValueBubble(self.prev, self.cur, value, fast_value)


@dc.dataclass(frozen=True)
class ValueBubble(Bubble):
    value: asm.Accessor | ArrayRef
    fast_value: asm.AssemblyExpression = None

    # If the ValueBubble represents a value that was pushed to the stack
    # get_fast may provide direct access to the underlying value.
    # However, it should be considered to be volatile.
    def get_fast(self, r_out):
        # Currently ArrayRef not allowed
        # Potentially the concept of a fast ArrayRef might make sense,
        # but I don't think it's useful / necessary.
        assert isinstance(self.value, asm.Accessor)
        if self.fast_value is not None:
            return self.fast_value
        return (yield from self.value.get(r_out))

@dc.dataclass
class LoopInfo:
    restore_point: StackPoint
    continue_label: asm.LabelRef
    break_label: asm.LabelRef

@dc.dataclass
class CatchInfo:
    prev_ap: ValueBubble
    contagious: ValueBubble
    label: asm.LabelRef


arith_map = {
    ast.Add: asm.Add, ast.Sub: asm.Sub, ast.Mul: asm.Mul,
    ast.Div: asm.Div, ast.Mod: asm.Mod
}

compare_map = {
    ast.Eq: asm.Heq, ast.Ne: asm.Hne,
    ast.Lt: asm.Hlt, ast.Gt: asm.Hgt,
    ast.Le: asm.Hle, ast.Ge: asm.Hge
}

halt_inversion = {
    asm.Heq: asm.Hne, asm.Hne: asm.Heq,
    asm.Hlt: asm.Hge, asm.Hgt: asm.Hle,
    asm.Hle: asm.Hgt, asm.Hge: asm.Hlt,
    asm.Hltu: asm.Hgeu, asm.Hgtu: asm.Hleu,
    asm.Hleu: asm.Hgtu, asm.Hgeu: asm.Hltu
}


@dc.dataclass
class CodeGen:
    func_decls: dict[ast.Ident, dict[tuple[ast.Type, ...], ast.FuncDeclaration]]
    global_decls: dict[str, ast.Declaration]
    word_size: int = 2
    stack_size: int = 50 # in words

    stack: StackPoint                                          = dc.field(init=False, default=StackPoint())
    allocated_arrays: list[ArrayRef]                           = dc.field(init=False, default_factory=list)
    global_vars: dict[str, asm.Accessor|ArrayRef]              = dc.field(init=False, default_factory=dict)
    local_vars: ChainMap[str, asm.Accessor|ArrayRef]           = dc.field(init=False, default_factory=ChainMap)
    return_address: asm.Accessor                               = dc.field(init=False, default=None)

    numbered_labels: dict[str, int]                            = dc.field(init=False, default_factory=dict)
    string_labels: dict[bytes, asm.LabelRef]                   = dc.field(init=False, default_factory=dict)
    const_data: dict[asm.LabelRef, asm.Directive]              = dc.field(init=False, default_factory=dict)
    state_data: dict[asm.LabelRef, asm.Directive]              = dc.field(init=False, default_factory=dict)

    func_queue: deque[ConcreteSignature]                       = dc.field(init=False, default_factory=deque)
    func_labels: dict[ConcreteSignature, asm.LabelRef]         = dc.field(init=False, default_factory=dict)
    func_table: dict[ConcreteSignature, list[asm.Instruction]] = dc.field(init=False, default_factory=dict)
    loop_info: deque[LoopInfo]                                 = dc.field(init=False, default_factory=deque)
    catch_info: CatchInfo                                      = dc.field(init=False, default=None)

    # TODO: I want to keep track of maximum static size (offset + static
    #  array size) seen for various checkpoints.
    #  Current thought is to have special mutable AssemblyExpression and
    #  maintain a sorted list with bisect.
    #  Removal seems it would be slightly awkward though.

    ap = asm.LabelRef('ap')
    fp = asm.LabelRef('fp')
    r0 = asm.LabelRef('r0')
    r1 = asm.LabelRef('r1')
    r2 = asm.LabelRef('r2')

    def __post_init__(self):
        if self.word_size < 2:
            raise CodeGenError('Word size must be at least 2 bytes', ())

        if ((self.stack_size + 5) * self.word_size) > self.max_signed:
            raise CodeGenError('Stack size too large', ())

        if not self.func_decls.get(ast.Ident.you('is_you'), {}).get(()):
            raise CodeGenError('Level is empty, expected entry point @is_you()', ())

        self.func_labels.update(stdlib.stdlib_funcs)
        self.label_for_func(ConcreteSignature(ast.Ident.you('is_you'), ()))

    @classmethod
    def from_program(cls, program: ast.Program, word_size, stack_size):
        func_decls = {}
        var_decls = {}
        for func_decl in program.func_decls:
            param_types = tuple(p.type for p in func_decl.params)
            func_decls.setdefault(func_decl.name, {})[param_types] = func_decl
        for var_decl in program.var_decls:
            var_decls[var_decl.var.name] = var_decl
        return cls(func_decls, var_decls, word_size, stack_size)

    def generate(self):
        self.make_funcs()
        yield b'%format word ' + str(self.word_size).encode('utf-8')
        yield b'%format output byte'
        yield b'%section state'
        yield b'ap: .word stack_start'
        yield b'fp: .word stack_end'
        yield b'r0: .word 0'
        yield b'r1: .word 0'
        yield b'r2: .word 0'
        yield b'stack_start:'
        yield from asm.ZeroDirective(asm.WordOffset(self.stack_size)).lines()
        yield b'.word all_is_win'
        yield b'stack_end:'
        for label, directive in self.state_data.items():
            yield from asm.Label(label).lines()
            yield from directive.lines()

        yield b'%section const'
        for string, label in self.string_labels.items():
            yield from asm.Label(label).lines()
            yield from asm.WordDirective(asm.IntLiteral(len(string))).lines()
            yield from asm.AsciiDirective(string).lines()
        for label, directive in self.const_data.items():
            yield from asm.Label(label).lines()
            yield from directive.lines()
        yield b'%section code'
        for code in self.func_table.values():
            yield from asm.lines(code)
        # yield from asm.Metadata('Standard library routines:').lines()
        yield from stdlib.stdlib_lines

    def make_funcs(self):
        while self.func_queue:
            csig = self.func_queue.pop()
            abstract_types = tuple(
                # TODO: maybe property abstract_type on concrete types?
                #  (including DataType), maybe even on ArrayType as well
                t if isinstance(t, DataType)
                else ArrayType(t.el_type, t.access != AccessMode.RW)
                for t in csig.arg_types
            )

            self.func_table[csig] = list(
                self.gen_func(csig, self.func_decls[csig.name][abstract_types])
            )

    def gen_func(self, csig: ConcreteSignature, func: ast.FuncDeclaration):
        assert self.stack == StackPoint(0)
        self.local_vars = ChainMap()

        # Reserve space for RA and passed arguments
        bubble = self.reserve_word()
        self.return_address = bubble.value
        for arg_type, param in zip(csig.arg_types, func.params, strict=True):
            arg_bubble = self.reserve_type(arg_type)
            self.local_vars[param.var.name] = arg_bubble.value
            bubble += arg_bubble

        yield asm.Metadata(f'Function {func.signature}:')
        yield asm.Label(self.func_labels[csig])
        yield from self.gen_block(func.body)

        # Return doesn't actually pop, it only restores ap.
        # Here we pop the arguments that we reserved space for.  This
        # should be equivalent to simply resetting self.stack.  No code
        # will be generated to deallocate arrays as any arrays passed to
        # the function belong to the caller.
        cleanup_args = list(self.pop(bubble))
        assert not cleanup_args, 'Should not require any instructions to be clean up arguments'
        assert self.stack == StackPoint(0)

    def gen_block(self, block: ast.Block):
        match block:
            case ast.CodeBlock():
                start_point = self.stack
                self.local_vars = self.local_vars.new_child()
                yield asm.Metadata(add_indent=1)

                exited, bubble = yield from self.gen_stmts(block.stmts)

                # So this is a little silly.  We don't want to generate
                # cleanup code when the block does its own cleanup, ie
                # if it exits.  We should be able to check exit_modes,
                # which will give the best information, but for some
                # reason I didn't include continue in exit_modes and I
                # can't remember why.  So I'm
                if exited or ExitMode.NONE not in block.exit_modes():
                    # Run the cleanup code of the pop generator for the
                    # purpose of bookkeeping, but discard instructions.
                    list(self.pop(bubble, static=False))
                else:
                    yield from self.pop(bubble, static=False)

                yield asm.Metadata(add_indent=-1)
                self.local_vars = self.local_vars.parents
                assert self.stack == start_point
            case ast.IfBlock():
                else_label = self.add_label('else')
                end_else = self.add_label('end_else')
                yield asm.Metadata('if block')
                yield from self.bool_expr_branch(block.cond, (), self.goto(else_label))
                yield from self.gen_block(block.body)
                # The goto is unnecessary if there's no else block.
                # But I feel like this sort of thing would be dealt with
                # better by a peephole optimizer.
                yield from self.goto(end_else)
                yield asm.Label(else_label)
                yield from self.gen_block(block.else_block)
                yield asm.Label(end_else)
            case ast.LoopBlock():
                loop_start = self.add_label('loop')
                loop_continue = self.add_label('continue')
                loop_break = self.add_label('break')
                yield asm.Metadata('loop block')
                yield asm.Label(loop_start)
                yield from self.bool_expr_branch(block.cond, (), self.goto(loop_break))
                self.loop_info.append(LoopInfo(self.stack, loop_continue, loop_break))
                yield from self.gen_block(block.body)
                self.loop_info.pop()
                yield asm.Label(loop_continue)
                yield from self.gen_block(block.cont)
                yield from self.goto(loop_start)
                yield asm.Label(loop_break)
            case ast.TryBlock():
                handler = self.add_label('try_handler')
                begin_try = self.add_label('begin_try')
                end_try = self.add_label('end_try')
                yield asm.Metadata('try block')
                if isinstance(block.handler, ast.CatchBlock):
                    # For catch, the simplest way I can think to manage
                    # the array pointer is simply to push old ap on the
                    # stack prior to try and restore in catch.
                    prev_ap = self.reserve_word()
                    yield from prev_ap.value.set(asm.State(self.ap))
                    contagious = self.reserve_byte()
                    assert self.catch_info is None
                    self.catch_info = CatchInfo(prev_ap, contagious, handler)
                    # If running the try "normally" would be defeat, we
                    # instead run in "contagious" mode, which makes it
                    # so that preempts are always taken and defeat jumps
                    # to the catch block.
                    yield from contagious.value.set(asm.IntLiteral(1))
                    yield asm.Jump(begin_try)
                    yield from contagious.value.set(asm.IntLiteral(0))
                    yield asm.Label(begin_try)
                else:
                    assert isinstance(block.handler, ast.UndoBlock)
                    yield asm.Jump(handler)
                yield from self.gen_block(block.body)
                yield from self.goto(end_try)
                yield asm.Label(handler)
                yield from self.gen_block(block.handler)
                yield asm.Label(end_try)
            case ast.UndoBlock():
                yield asm.Metadata('undo block')
                yield from self.gen_block(block.body)
            case ast.CatchBlock():
                yield asm.Metadata('catch block')

                # Do not allow catch block to run unless contagious
                is_contagious = yield from self.pop_value(self.r1, self.catch_info.contagious)
                yield asm.Heq(is_contagious, asm.IntLiteral(0))
                yield from self.catch_info.prev_ap.value.to(self.ap)
                yield from self.pop(self.catch_info.prev_ap)
                self.catch_info = None
                yield from self.gen_block(block.body)
            case ast.PreemptBlock():
                do_preempt = self.add_label('do_preempt')
                end_preempt = self.add_label('end_preempt')
                yield asm.Metadata('preempt block')
                yield asm.Jump(do_preempt)
                is_contagious = yield from self.catch_info.contagious.value.get(self.r1)
                yield asm.Hne(is_contagious, asm.IntLiteral(0))
                yield from self.goto(end_preempt)
                yield asm.Label(do_preempt)
                yield from self.gen_block(block.body)
                yield asm.Label(end_preempt)
            case unknown:
                raise InternalCompilerError(f'Unrecognized block {unknown}')

    def gen_stmts(self, stmts)->asm.InstrGen[tuple[bool, Bubble]]:
        var_bubble = Bubble(self.stack, self.stack)
        for stmt in stmts:
            yield asm.Metadata(f'Statement @ {stmt.span}: {stmt.__class__.__name__}')
            match stmt:
                case ast.Expression():
                    bubble = yield from self.eval_expr(self.r1, stmt, keep=False)
                    yield from self.pop(bubble)
                case ast.Block():
                    yield from self.gen_block(stmt)
                case ast.Declaration():
                    bubble = yield from self.push_expr(self.r1, stmt.init)
                    self.local_vars[stmt.var.name] = bubble.value
                    var_bubble += bubble
                case ast.Assignment():
                    # This includes both regular and IncAssignments
                    if isinstance(stmt.lookup, ast.VariableLookup):
                        if isinstance(stmt, ast.IncAssignment):
                            stmt = stmt.type_equiv_assignment()
                        is_global, access = self.lookup_var(stmt.lookup.var)
                        value = yield from self.get_expr_value(self.r1, stmt.expr)
                        yield from access.set(value)
                    else:
                        bin_op = None
                        if isinstance(stmt, ast.IncAssignment):
                            bin_op = stmt.bin_op
                        assert isinstance(stmt.lookup, ast.ArrayLookup)
                        yield from self.array_assignment(
                            stmt.lookup.source, stmt.lookup.index,
                            stmt.expr, bin_op
                        )
                case ast.ReturnStatement():
                    if stmt.value is not None:
                        retval = yield from self.get_expr_value(self.r0, stmt.value)
                        ra = yield from self.return_address.get(self.r1)
                        with self.at_offset(0):
                            bubble = self.reserve_type(stmt.value.type)
                            yield from bubble.value.set(retval)
                    else:
                        ra = yield from self.return_address.get(self.r1)
                    # Deallocate all arrays currently in scope
                    yield from self.reset_ap(0)
                    # Jump to return address
                    yield from self.goto(ra)
                    return True, var_bubble
                case ast.BreakStatement():
                    info = self.loop_info[-1]
                    yield from self.reset_ap(info.restore_point.array_num)
                    yield from self.goto(info.break_label)
                    return True, var_bubble
                case ast.ContinueStatement():
                    info = self.loop_info[-1]
                    yield from self.reset_ap(info.restore_point.array_num)
                    yield from self.goto(info.continue_label)
                    return True, var_bubble
                case unknown:
                    raise InternalCompilerError(f'Unrecognized statement {unknown}')
        return False, var_bubble

    def push_expr(self, r_use: asm.LabelRef, expr: ast.Expression)->asm.InstrGen[ValueBubble]:
        expected_size = self.frame_size(expr.type)
        if isinstance(expr, ast.ByteToInt):
            # Slight optimization, and will hopefully also allow byte to
            # int coercions in tail calls when/if I add TCE.

            # Clear a word and push the expression to the top byte
            # Assumes little endian and backwards-growing stack.
            # Only difference if it were forward growing is we'd need to
            # zero a word afterwards rather than before.
            bubble = self.reserve_word()
            assert bubble.frame_size == expected_size
            yield from bubble.value.set(asm.IntLiteral(0))
            self.stack = self.stack.add(offset=-1)
            byte_bubble = yield from self.push_expr(r_use, expr.expr)
            return bubble.with_value(bubble.value, byte_bubble.fast_value)

        bubble = yield from self.eval_expr(r_use, expr, keep=True)
        if bubble.frame_size == expected_size:
            # Bubble was already pushed and has correct size
            return bubble
        elif isinstance(expr.type, ArrayType):
            # If array is in the bubble, and it's the wrong size it must
            # be a reference, so the bubble should be vacuous.
            assert isinstance(bubble.value, ArrayRef) and bubble.vacuous
            new_bubble = self.reserve_type(bubble.value.type)
            length = yield from bubble.value.length.get(r_use)
            yield from new_bubble.value.length.set(length)
            origin = yield from bubble.value.origin.get(r_use)
            yield from new_bubble.value.origin.set(origin)
            return new_bubble
        else:
            # The bubble is wrong size, so pop and push a properly sized
            # bubble in its place.
            # Bubble is not necessarily vacuous, such as in the case of
            # IntToByte.  IntToByte is a rather unfortunate case, since
            # due to the little-endian byteorder and backwards-growing
            # stack, we do need to move the value up, even if it is on
            # the stack already.  We can't use a forward-growing stack
            # because it makes dynamically deallocating arrays awkward.
            # Could make endianness configurable in Sphinx so we can use
            # big endian, but that'd make access_byte() a pain.
            # TODO: I mean we could just deal with IntToByte by by doing
            #  get_expr_value and pushing a byte for that...
            assert isinstance(bubble.value, asm.Accessor)
            value = yield from bubble.get_fast(r_use)
            yield from self.pop(bubble)
            return (yield from self.push_value(expr.type, value))

    def get_expr_value(self, r_out: asm.LabelRef, expr: ast.Expression)->asm.InstrGen[asm.AssemblyExpression]:
        bubble = yield from self.eval_expr(r_out, expr, keep=False)
        return (yield from self.pop_value(r_out, bubble))

    def eval_expr(self, r_out: asm.LabelRef, expr: ast.Expression, keep: bool)->asm.InstrGen[ValueBubble]:
        # If keep is False, the Accessor/ArrayRef in the ValueBubble is
        # whatever accessor is most convenient to access the value.
        # If keep is True, the value is pushed to the stack if this
        # "most convenient" accessor is considered volatile: ie if it is
        # the r_out register or a non-const global variable, as these
        # locations may be mutated evaluating subsequent expressions.
        # In the future, the concept of volatility may become more
        # strict if for instance I add x++ as an expression, which would
        # make all non-const variables volatile in this sense.

        # May return from match-case if non-volatile Bubble is obtained.
        # Otherwise, set result (default r_out).  If it is Immediate, it
        # will be treated as non-volatile, otherwise copied to stack.
        result: asm.AssemblyExpression = asm.State(r_out)
        match expr:
            case ast.ByteValue():
                result = asm.IntLiteral(expr.data, is_byte=True)
            case ast.IntValue():
                result = asm.IntLiteral(expr.data)
            case ast.BoolValue():
                result = asm.IntLiteral(int(expr.data))
            case ast.StringValue():
                result = self.label_for_string(expr.data)
            case ast.BoolToByte() | ast.ByteToInt():
                return (yield from self.eval_expr(r_out, expr.expr, keep))
            case ast.Volatile():
                bubble = yield from self.eval_expr(r_out, expr.expr, keep)
                return bubble.with_value(bubble.value.make_const())
            case ast.IntToByte():
                # In many cases, f(word) = f(byte) (mod 256)
                # but we can't rely on that in general -- add option?
                bubble = yield from self.eval_expr(r_out, expr.expr, keep)
                # Notice that the bubble has not changed size, we have
                # only switched to byte access.
                return bubble.with_value(bubble.value.access_byte())
            case ast.IntToBool():
                value = yield from self.get_expr_value(r_out, expr.expr)
                if isinstance(value, asm.IntLiteral):
                    # Probably should be handled entirely by TC, but I
                    # think some cases like array length aren't dealt
                    # with properly yet, so might as well do it here.
                    return self.vacpack(asm.IntLiteral(int(bool(value.data))))

                bool_normalized = self.add_label('bool_normalized')

                # The halt propagation here requires value in r_out
                yield from value.to(r_out)

                yield asm.Jump(bool_normalized)
                yield asm.Hleu(asm.State(r_out), asm.IntLiteral(1))
                yield asm.Mov(r_out, asm.IntLiteral(1))
                yield asm.Label(bool_normalized)
                yield asm.Hgtu(asm.State(r_out), asm.IntLiteral(1))
            case ast.StringToByteArray():
                bubble = self.reserve_type(ConcreteArrayType(
                    DataType.BYTE, AccessMode.RC
                ))

                # TODO: optimization in case of Immediate string
                string = yield from self.get_expr_value(self.r1, expr.expr)
                yield asm.Lwc(self.r0, string)
                yield from bubble.value.length.set(asm.State(self.r0))
                yield asm.Add(self.r1, string, asm.IntLiteral(self.word_size))
                yield from bubble.value.origin.set(asm.State(self.r1))
                return bubble
            case ast.BinaryArithmeticOp():
                # If expr.right is simple enough, we could do keep=False
                left_bubble = yield from self.eval_expr(self.r0, expr.left, keep=True)
                right = yield from self.get_expr_value(self.r1, expr.right)
                left = yield from self.pop_value(self.r0, left_bubble)
                yield from self.arith_op_reg_arg(type(expr), r_out, left, right)
            case ast.Unary():
                value = yield from self.get_expr_value(r_out, expr.arg)
                yield from self.un_op_reg_arg(type(expr), r_out, value)
            case ast.BooleanOp():
                if keep:
                    bubble = self.reserve_type(expr.type)
                else:
                    bubble = self.vacpack(asm.State(r_out))
                yield from self.bool_expr_branch(
                    expr,
                    tuple(bubble.value.set(asm.IntLiteral(1))),
                    tuple(bubble.value.set(asm.IntLiteral(0)))
                )
                return bubble
            case ast.VariableLookup():
                is_global, access = self.lookup_var(expr.var)
                if keep and is_global and not expr.var.const:
                    # Array variables are always const, so this won't
                    # be an ArrayRef
                    assert isinstance(access, asm.Accessor)
                    result = yield from access.get(r_out)
                else:
                    return self.vacpack(access)
            case ast.FuncCall():
                return (yield from self.eval_func_call(expr.type, expr.func, expr.args))
            case ast.ArrayLiteral():
                # If array is const and all values are primitive, we can
                # just turn it into a const global.
                if expr.type.const and all(isinstance(v, ast.PrimitiveValue) for v in expr.values):
                    return self.vacpack(self.make_global(expr, const=True))

                # TODO: we may need to ensure length is not TOO long,
                #  depending on how the stack overflow detection works.
                length = len(expr.values)
                el_type = expr.type.el_type
                length_bubble = self.reserve_word()
                yield from length_bubble.value.set(asm.IntLiteral(length))
                length_bubble = length_bubble.with_value(asm.IntLiteral(length))
                origin_bubble = self.reserve_word()
                yield from origin_bubble.value.set(asm.State(self.ap))
                static_size = self.array_size(el_type, length)
                # We must advance ap before, not after we write values
                # It should be fine not to update self.stack yet though.
                yield asm.Metadata('Array allocation (ArrayLiteral)')
                yield asm.Add(self.ap, asm.State(self.ap), asm.IntLiteral(static_size))
                if el_type == DataType.BOOL:
                    foundation = self.pack_bools([
                        isinstance(el_expr, ast.BoolValue) and el_expr.data
                        for el_expr in expr.values
                    ])

                    # TODO: clean this mess of a loop up
                    #  Also would be good to avoid the store and load in
                    #  more cases than just the first time.
                    offset = -static_size
                    for byte in foundation:
                        prev = asm.IntLiteral(byte)
                        for i, el_expr in zip(range(8), expr.values):
                            if not isinstance(el_expr, ast.BoolValue):
                                if not isinstance(prev, asm.IntLiteral):
                                    yield asm.Sbso(asm.State(self.ap), asm.IntLiteral(offset), prev)
                                element = yield from self.get_expr_value(self.r0, el_expr)
                                if i != 0:
                                    yield asm.Asl(self.r0, element, asm.IntLiteral(i))
                                    element = asm.State(self.r0)
                                if not isinstance(prev, asm.IntLiteral):
                                    yield asm.Lbso(self.r1, asm.State(self.ap), asm.IntLiteral(offset))
                                    prev = asm.State(self.r1)
                                yield asm.Or(self.r1, prev, element)
                                prev = asm.State(self.r1)
                        yield asm.Sbso(asm.State(self.ap), asm.IntLiteral(offset), prev)
                        offset += 1
                    assert offset == 0
                else:
                    if el_type.byte_sized:
                        stride = 1
                        so_instr = asm.Sbso
                    else:
                        stride = self.word_size
                        so_instr = asm.Swso

                    offset = -static_size
                    for el_expr in expr.values:
                        element = yield from self.get_expr_value(r_out, el_expr)
                        yield so_instr(asm.State(self.ap), asm.IntLiteral(offset), element)
                        offset += stride
                    assert offset == 0

                access_mode = AccessMode.R if expr.type.const else AccessMode.RW
                return self.create_new_stack_array(
                    ConcreteArrayType(expr.type.el_type, access_mode),
                    origin_bubble=origin_bubble, length_bubble=length_bubble,
                    static_size=static_size
                )
            case ast.ArrayInitializer():
                # TODO: stack overflow / negative length detection
                length_bubble = yield from self.push_expr(r_out, expr.length)
                length = yield from length_bubble.get_fast(r_out)
                origin_bubble = self.reserve_word()
                yield from origin_bubble.value.set(asm.State(self.ap))
                size = yield from self.get_array_size(r_out, expr.type.el_type, length)
                yield asm.Metadata('Array allocation (ArrayInitializer)')
                yield asm.Add(self.ap, asm.State(self.ap), size)
                # It should always be RW, but we'll leave it up to the
                # typechecker to make such decisions.
                access_mode = AccessMode.R if expr.type.const else AccessMode.RW
                return self.create_new_stack_array(
                    ConcreteArrayType(expr.type.el_type, access_mode),
                    origin_bubble=origin_bubble, length_bubble=length_bubble
                )
            case ast.ArrayLookup():
                yield from self.array_lookup(r_out, expr.source, expr.index)
            case ast.LengthLookup():
                if isinstance(expr.source.type, ArrayType):
                    # bubble, ref = yield from self.get_array_reference(expr.source)
                    bubble = yield from self.eval_expr(r_out, expr.source, keep=False)
                    assert isinstance(bubble.value, ArrayRef)
                    result = yield from bubble.value.length.get(r_out)
                    yield from self.pop(bubble)
                elif expr.source.type == DataType.STRING:
                    source = yield from self.get_expr_value(r_out, expr.source)
                    yield asm.Lwc(r_out, source)
                else:
                    assert False
            case unknown:
                raise InternalCompilerError(f'Unrecognized expression {unknown}')

        if not keep or isinstance(result, asm.Immediate):
            return self.vacpack(result)

        assert isinstance(expr.type, DataType)
        return (yield from self.push_value(expr.type, result))

    def eval_func_call(self, ret_type: ast.DataType, name: ast.Ident,
                       args: tuple[ast.Expression, ...])->asm.InstrGen[ValueBubble]:

        # Provide an opportunity to jump to catch block for every defeat
        # function call.
        if self.catch_info is not None and name.flavor == ast.Flavor.DEFEAT:
            yield asm.Jump(self.catch_info.label)

        if name == ast.Ident('print') and len(args) == 1:
            if args[0].type == DataType.BYTE:
                arg = yield from self.get_expr_value(self.r1, args[0])
                yield asm.Yield(arg)
                return self.reserve_type(DataType.VOID)
        elif name == ast.Ident('println'):
            # TODO: erm, this isn't really quite right...
            #  we only want to do the translation from println to print
            #  if the signature matches one of ours.
            if len(args) != 0:
                result = yield from self.eval_func_call(DataType.VOID, ast.Ident('print'), args)
                yield from self.pop(result)
            yield asm.Yield(asm.IntLiteral(ord('\n'), is_byte=True))
            return self.reserve_type(DataType.VOID)
        elif name == ast.Ident.defeat('is_defeat') and len(args) == 0:
            yield asm.Halt()
            return self.reserve_type(DataType.VOID)
        elif name == ast.Ident.defeat('truth_is_defeat') and len(args) == 1:
            if args[0].type == DataType.BOOL:
                yield from self.truth_is_defeat(args[0])
                return self.reserve_type(DataType.VOID)

        end_call = self.add_label('end_call')

        # Offset prior to the call
        offset = self.stack.offset

        # Push return address
        bubble = self.reserve_word()
        yield from bubble.value.set(end_call)

        arg_types = []
        for arg in args:
            arg_bubble = yield from self.push_expr(self.r1, arg)
            if isinstance(arg.type, DataType):
                arg_types.append(arg.type)
            else:
                arg_types.append(arg_bubble.value.type)
            bubble += arg_bubble

        label = self.label_for_func(ConcreteSignature(name, tuple(arg_types)))
        yield asm.Add(self.fp, asm.State(self.fp), asm.IntLiteral(-offset))
        yield from self.goto(label)
        yield asm.Label(end_call)
        yield asm.Add(self.fp, asm.State(self.fp), asm.IntLiteral(offset))
        yield from self.pop(bubble)
        return self.reserve_type(ret_type)

    def lookup_var(self, var: ast.Variable):
        # Locals access always considered safe
        if access := self.local_vars.get(var.name):
            return False, access
        # Add global variable if it doesn't exist
        if not (access := self.global_vars.get(var.name)):
            # Kinda awkward...  Not sure I like how make_global's const
            # works.  Maybe for arrays it should look ignore passed
            # const and look only at expr.type.const?  IDK.
            if isinstance(var.type, ArrayType):
                const = var.type.const
            else:
                const = var.const
            access = self.make_global(
                self.global_decls[var.name].init,
                const, label_prefix=f'var_{var.name}'
            )
            self.global_vars[var.name] = access
        return True, access

    def make_global(self, initializer: ast.Expression, const: bool, label_prefix='data') -> asm.Accessor | ArrayRef:
        match initializer:
            case ast.PrimitiveValue():
                try:
                    next(self.eval_expr(self.r1, initializer, keep=True))
                except StopIteration as ret:
                    assert ret.value.vacuous
                    assert isinstance(ret.value.value, asm.Immediate)
                    # Constant immediate values don't need to be stored
                    # anywhere.  They may be used as is.
                    if const:
                        return ret.value.value
                    label = self.add_label(label_prefix)
                    if initializer.type.byte_sized:
                        self.state_data[label] = asm.ByteDirective(ret.value.value)
                        return asm.StateByte(label)
                    else:
                        self.state_data[label] = asm.WordDirective(ret.value.value)
                        return asm.State(label)
                assert False
            case ast.ArrayInitializer():
                if isinstance(initializer.length, ast.IntValue):
                    el_type = initializer.type.el_type
                    length = initializer.length.data & self.max_unsigned
                    return self.add_global_array(
                        const, el_type, label_prefix, length, asm.ZeroDirective(
                            asm.IntLiteral(self.array_size(el_type, length))
                        )
                    )
            case ast.ArrayLiteral():
                # Using const=True relies on that we don't actually put
                # const immediates into const
                values = [self.make_global(v, const=True)
                          for v in initializer.values]
                assert all(isinstance(v, asm.Immediate) for v in values)
                if initializer.type.el_type == DataType.BOOL:
                    directive = asm.ByteDirective(*map(
                        asm.IntLiteral,
                        self.pack_bools([bool(lit.data) for lit in values])
                    ))
                elif initializer.type.el_type.byte_sized:
                    directive = asm.ByteDirective(*values)
                else:
                    directive = asm.WordDirective(*values)

                # Activate maximum paranoia mode
                scale = {asm.WordDirective: self.word_size,
                         asm.ByteDirective: 1}[type(directive)]
                assert scale * len(directive.items) == self.array_size(
                    initializer.type.el_type, len(values)
                )

                return self.add_global_array(
                    const, initializer.type.el_type,
                    label_prefix, len(values), directive
                )
        # We treat it as a CodeGenError rather than an internal error if
        # we cannot evaluate the initializer at compile time, implying
        # that it's the user's fault.
        # Really the check for this stuff should be in the typechecker
        # probably, but I don't think it does currently.
        raise CodeGenError(
            'Expression cannot be evaluated at compile time',
            initializer.span
        )

    @staticmethod
    def pack_bools(bools, *, bit_size=8):
        result = []
        for i, b in enumerate(bools):
            bit = i % bit_size
            if bit == 0:
                result.append(0)
            result[-1] |= b << bit
        return result

    def add_global_array(self, const, el_type: DataType, label_prefix, length, directive):
        length = min(length, self.max_length(el_type))
        label = self.add_label(label_prefix)
        data_dict = self.const_data if const else self.state_data
        access = AccessMode.RC if const else AccessMode.RW
        data_dict[label] = directive
        return ArrayRef(ConcreteArrayType(el_type, access), label, asm.IntLiteral(length))

    # if_true and if_false are iterables of instructions.
    # these may be duplicated, and so should be short.
    # If a sequence of instructions ends in an unconditional jump, it is
    # treated specially by omitting unreachable jumps which would
    # otherwise be added to jump to the end.
    def bool_expr_branch(self, expr, if_true, if_false):
        if_true = tuple(if_true)
        if_false = tuple(if_false)
        true_end_goto = self.is_goto(if_true[-2:])
        false_end_goto = self.is_goto(if_false[-2:])
        if instr := compare_map.get(type(expr)):
            # TODO: equality ops can have boolean arguments, maybe could
            #  use bool_expr_jump in that case?
            compare_is_true = self.add_label('compare_is_true')
            compare_end = self.add_label('compare_end')
            left_bubble = yield from self.eval_expr(self.r0, expr.left, keep=True)
            right = yield from self.get_expr_value(self.r1, expr.right)
            left = yield from self.pop_value(self.r0, left_bubble)
            yield asm.Jump(compare_is_true)
            yield instr(left, right)
            yield from if_false
            if not false_end_goto:
                yield from self.goto(compare_end)
            yield asm.Label(compare_is_true)
            yield halt_inversion[instr](left, right)
            yield from if_true
            if not false_end_goto:
                yield asm.Label(compare_end)
        elif type(expr) is ast.Not:
            yield from self.bool_expr_branch(expr.arg, if_false, if_true)
        elif type(expr) is ast.And:
            left_is_true = self.add_label('left_is_true')
            and_end = self.add_label('and_end')
            yield from self.bool_expr_branch(
                expr.left, self.goto(left_is_true),
                if_false if false_end_goto else
                if_false + tuple(self.goto(and_end))
            )
            yield asm.Label(left_is_true)
            yield from self.bool_expr_branch(expr.right, if_true, if_false)
            if not false_end_goto:
                yield asm.Label(and_end)
        elif type(expr) is ast.Or:
            left_is_false = self.add_label('left_is_false')
            or_end = self.add_label('or_end')
            yield from self.bool_expr_branch(
                expr.left,
                if_true if true_end_goto else
                if_true + tuple(self.goto(or_end)),
                self.goto(left_is_false)
            )
            yield asm.Label(left_is_false)
            yield from self.bool_expr_branch(expr.right, if_true, if_false)
            if not true_end_goto:
                yield asm.Label(or_end)
        elif type(expr) is ast.BoolValue:
            if expr.data:
                yield from if_true
            else:
                yield from if_false
        elif expr.type == DataType.BOOL:
            # Ensure there is no BooleanOp which is unimplemented by
            # this method, avoiding infinite recursion.
            assert not isinstance(expr, ast.BooleanOp)

            # Unwrap typecast to bool because there's no need to force
            # true to be 1 here.
            if type(expr) is ast.IntToBool:
                expr = expr.expr

            expr_is_true = self.add_label('is_true')
            bool_end = self.add_label('bool_end')
            value = yield from self.get_expr_value(self.r1, expr)
            yield asm.Jump(expr_is_true)
            yield asm.Hne(value, asm.IntLiteral(0))
            yield from if_false
            if not false_end_goto:
                yield from self.goto(bool_end)
            yield asm.Label(expr_is_true)
            yield asm.Heq(value, asm.IntLiteral(0))
            yield from if_true
            if not false_end_goto:
                yield asm.Label(bool_end)
        else:
            assert False

    def truth_is_defeat(self, expr):
        # Some cases of !truth_is_defeat can be nicely optimized, others
        # not so much.
        # TODO: Potential future optimization - substitute things like
        #  !truth_is_defeat(not (x == y)) with !truth_is_defeat(x != y)
        #  not is expensive.

        if instr := compare_map.get(type(expr)):
            left_bubble = yield from self.eval_expr(self.r0, expr.left, keep=True)
            right = yield from self.get_expr_value(self.r1, expr.right)
            left = yield from self.pop_value(self.r0, left_bubble)
            yield instr(left, right)
        elif type(expr) is ast.Or:
            yield from self.truth_is_defeat(expr.left)
            yield from self.truth_is_defeat(expr.right)
        elif type(expr) is ast.BoolValue:
            if expr.data: yield asm.Halt()
        elif expr.type == DataType.BOOL:
            if type(expr) is ast.IntToBool:
                expr = expr.expr
            # I think get_expr_value is better than bool_expr_branch
            # here in most cases, but I don't know.
            value = yield from self.get_expr_value(self.r1, expr)
            yield asm.Hne(value, asm.IntLiteral(0))
        else:
            assert False

    def arith_op_reg_arg(self, op_type, r_out:asm.LabelRef,
                         arg_left:asm.AssemblyExpression,
                         arg_right:asm.AssemblyExpression):
        if instr := arith_map.get(op_type):
            if op_type in {ast.Div, ast.Mod}:
                yield asm.Jump(div_allowed := self.add_label('div_allowed'))
                yield asm.Hne(arg_right, asm.IntLiteral(0))
                yield from self.goto(stdlib.division_by_zero)
                yield asm.Label(div_allowed)
            yield instr(r_out, arg_left, arg_right)
        else:
            assert False

    def un_op_reg_arg(self, op_type, r_out:asm.LabelRef, arg_in:asm.AssemblyExpression):
        if op_type is ast.Pos:
            if arg_in != asm.State(r_out):
                yield asm.Mov(r_out, arg_in)
        elif op_type is ast.Neg:
            yield asm.Sub(r_out, asm.IntLiteral(0), arg_in)
        elif op_type is ast.Not:
            # Requirement: boolean value MUST be either 0 or 1
            yield asm.Sub(r_out, asm.IntLiteral(1), arg_in)
        else:
            assert False

    @property
    def max_signed(self):
        return (1 << (8 * self.word_size - 1)) - 1

    @property
    def max_unsigned(self):
        return (1 << (8 * self.word_size)) - 1

    def max_length(self, el_type: DataType):
        if el_type.byte_sized:
            return self.max_signed
        return self.max_signed // self.word_size

    # Probably unnecessary because we ensure stack size isn't too big
    # Just check for stack overflow instead.
    # But be aware of overflow when checking stack overflow I guess.
    # I guess maybe if el_type is not byte size, then it's safest to do
    # two checks, one that length is sane, and the other one for stack
    # overflow, but if it is byte sized then only check stack overflow.
    # def check_length(self, el_type: DataType, length_arg: asm.AssemblyExpression):
    #     safe_length = self.add_label('safe_length')
    #     yield asm.Jump(safe_length)
    #     if el_type.byte_sized:
    #         yield asm.Hge(length_arg, asm.IntLiteral(0))
    #     else:
    #         yield asm.Hleu(length_arg, asm.IntLiteral(self.max_signed // self.word_size))
    #     yield from self.goto(stdlib.out_of_bounds)
    #     yield asm.Label(safe_length)

    def check_index(self, idx_arg: asm.AssemblyExpression, length_arg: asm.AssemblyExpression):
        index_in_bounds = self.add_label('index_in_bounds')

        # The length should always be positive signed.
        # We produce error if index is greater than or equal to length
        # unsigned, ie it is greater than length signed or less than 0
        yield asm.Jump(index_in_bounds)
        yield asm.Hltu(idx_arg, length_arg)
        yield from self.goto(stdlib.out_of_bounds)
        yield asm.Label(index_in_bounds)

    def array_lookup(self, r_out, src_expr, idx_expr):
        # In the case of string, we need keep for the source bubble.
        # Doesn't matter in the case of arrays, eval_expr shouldn't ever
        # copy array references.
        bubble = yield from self.eval_expr(self.r2, src_expr, keep=True)
        index = yield from self.get_expr_value(self.r1, idx_expr)
        if src_expr.type == DataType.STRING:
            source = yield from self.pop_value(self.r2, bubble)
            yield asm.Lwc(self.r0, source)
            yield from self.check_index(index, asm.State(self.r0))

            # If either source_arg or idx_arg is immediate, this
            # add could be combined into the lbco if I wanted.
            # But I'd need to be aware that source_arg might be
            # a LabelRef, as in the case of "hello"[0]
            yield asm.Add(self.r1, index, asm.IntLiteral(self.word_size))
            yield asm.Lbco(r_out, source, asm.State(self.r1))
            return asm.State(r_out)
        elif isinstance(src_expr.type, ArrayType):
            length = yield from bubble.value.length.get(self.r0)
            yield from self.check_index(index, length)
            origin = yield from bubble.value.origin.get(self.r0)

            section = bubble.value.section
            if src_expr.type.el_type == DataType.BOOL:
                yield asm.And(self.r2, index, asm.IntLiteral(7))
                yield asm.Asr(self.r1, index, asm.IntLiteral(3))
                yield section.lbo(self.r1, origin, asm.State(self.r1))
                yield asm.Asr(self.r1, asm.State(self.r1), asm.State(self.r2))
                yield asm.And(r_out, asm.State(self.r1), asm.IntLiteral(1))
            elif src_expr.type.el_type.byte_sized:
                yield section.lbo(r_out, origin, index)
            else:
                yield asm.Mul(self.r1, index, asm.IntLiteral(self.word_size))
                yield section.lwo(r_out, origin, asm.State(self.r1))

            yield from self.pop(bubble)
            return asm.State(r_out)
        else:
            assert False

    def array_assignment(self, src_expr, idx_expr, rhs_expr, bin_op=None):
        assert isinstance(src_expr.type, ArrayType)
        array_bubble = yield from self.eval_expr(self.r2, src_expr, keep=True)
        section = array_bubble.value.section
        if src_expr.type.el_type == DataType.BOOL:
            # No incremental assignments currently exist for BOOL
            # And I'll be damned to add any!
            assert bin_op is None

            # The plan:
            #  b = i / 8
            #  n = i % 8
            #  a[b] = (a[b] & ~(1<<n)) | (rhs<<n)
            # Note: Unlike incremental assignments, we need a[b] AFTER
            # rhs has been evaluated, rather than before.

            index = yield from self.get_expr_value(self.r1, idx_expr)
            length = yield from array_bubble.value.length.get(self.r0)
            yield from self.check_index(index, length)
            yield asm.Asr(self.r2, index, asm.IntLiteral(3))
            offset_bubble = self.reserve_word()
            yield from offset_bubble.value.set(asm.State(self.r2))
            yield asm.And(self.r1, index, asm.IntLiteral(7))
            bit_bubble = self.reserve_byte()
            yield from bit_bubble.value.set(asm.State(self.r1))

            rhs = yield from self.get_expr_value(self.r2, rhs_expr)
            origin = yield from array_bubble.value.origin.get(self.r0)
            offset = yield from offset_bubble.value.get(self.r1)
            yield section.lbo(self.r0, origin, offset)
            # Alternatively here we could do the 1<<bit first and then
            # multiply it by rhs.  This would allow us to keep 1<<bit on
            # the stack rather than bit.  Might be useful if we do end
            # up wanting to add incremental boolean operators like ^=
            bit = yield from self.pop_value(self.r1, bit_bubble)
            yield asm.Asl(self.r2, rhs, bit)
            yield asm.Asl(self.r1, asm.IntLiteral(1), bit)
            yield asm.Xor(self.r1, asm.State(self.r1), asm.IntLiteral(-1))
            yield asm.And(self.r1, asm.State(self.r0), asm.State(self.r1))
            yield asm.Or(self.r2, asm.State(self.r1), asm.State(self.r2))
            origin = yield from array_bubble.value.origin.get(self.r0)
            offset = yield from self.pop_value(self.r1, offset_bubble)
            yield section.sbo(origin, offset, asm.State(self.r2))
            yield from self.pop(array_bubble)
            return  # We've done our own cleanup here
        elif src_expr.type.el_type.byte_sized:
            offset_bubble = yield from self.eval_expr(self.r1, idx_expr, keep=True)
            offset = yield from offset_bubble.get_fast(self.r1)
            length = yield from array_bubble.value.length.get(self.r0)
            yield from self.check_index(offset, length)
            lo_instr = section.lbo
            so_instr = section.sbo
        else:
            index = yield from self.get_expr_value(self.r1, idx_expr)
            length = yield from array_bubble.value.length.get(self.r0)
            yield from self.check_index(index, length)
            yield asm.Mul(self.r1, index, asm.IntLiteral(self.word_size))
            offset = asm.State(self.r1)
            offset_bubble = self.reserve_word()
            yield from offset_bubble.value.set(offset)
            lo_instr = section.lwo
            so_instr = section.swo
        # Here we have offset_bubble, offset:r1, lo_instr, so_instr

        if bin_op is None:
            rhs = yield from self.get_expr_value(self.r2, rhs_expr)
        else:
            origin = yield from array_bubble.value.origin.get(self.r0)
            yield lo_instr(self.r0, origin, offset)
            lhs_bubble = yield from self.push_value(src_expr.type.el_type, asm.State(self.r0))
            increment = yield from self.get_expr_value(self.r2, rhs_expr)
            lhs = yield from self.pop_value(self.r0, lhs_bubble)
            yield from self.arith_op_reg_arg(bin_op, self.r2, lhs, increment)
            rhs = asm.State(self.r2)
        # Here we have rhs:r2

        origin = yield from array_bubble.value.origin.get(self.r0)
        offset = yield from self.pop_value(self.r1, offset_bubble)
        yield so_instr(origin, offset, rhs)
        yield from self.pop(array_bubble)


    def add_label(self, name):
        suffix = self.numbered_labels.get(name, 0)
        self.numbered_labels[name] = suffix + 1
        return asm.LabelRef(f'{name}_{suffix}')

    def label_for_string(self, data):
        try:
            return self.string_labels[data]
        except KeyError:
            label = self.add_label('string')
            self.string_labels[data] = label
            return label

    def label_for_func(self, sig: ConcreteSignature):
        try:
            return self.func_labels[sig]
        except KeyError:
            label = self.add_label(f'func_{sig.name.base_name}')
            self.func_labels[sig] = label
            self.func_queue.appendleft(sig)
            return label

    def mark(self, label_ref):
        yield asm.Label(label_ref)
        return label_ref

    def goto(self, addr):
        # unconditional jump
        yield asm.Jump(addr)
        yield asm.Halt()

    def is_goto(self, instructions):
        # TODO: ignore metadata?
        return (len(instructions) == 2 and
                isinstance(instructions[0], asm.Jump) and
                isinstance(instructions[1], asm.Halt))

    # Vacuum-pack your bytes so you can eat them later!
    def vacpack(self, value, fast_value=None):
        return ValueBubble(self.stack, self.stack, value, fast_value)

    def reserve_byte(self)->ValueBubble:
        prev = self.stack
        cur = prev.add(offset=1)
        self.stack = cur
        return ValueBubble(prev, cur, asm.IndirectByte(
            asm.Section.STATE, asm.State(self.fp),
            asm.IntLiteral(-cur.offset)
        ))

    def reserve_word(self)->ValueBubble:
        prev = self.stack
        cur = prev.add(offset=self.word_size)
        self.stack = cur
        return ValueBubble(prev, cur, asm.Indirect(
            asm.Section.STATE, asm.State(self.fp),
            asm.IntLiteral(-cur.offset)
        ))

    def reserve_type(self, val_type: ConcreteType):
        # NOTE: reserving an array type does NOT allocate the array
        # Use create_new_stack_array for that.
        if isinstance(val_type, ConcreteArrayType):
            length_bubble = self.reserve_word()
            origin_bubble = self.reserve_word()
            return (length_bubble + origin_bubble).with_value(ArrayRef(
                val_type, origin_bubble.value, length_bubble.value
            ))
        assert isinstance(val_type, DataType)
        if val_type.byte_sized:
            return self.reserve_byte()
        if val_type == DataType.VOID:
            return self.vacpack(asm.VoidAccessor())
        return self.reserve_word()

    def array_size(self, data_type, length):
        assert isinstance(data_type, DataType)
        if data_type == DataType.BOOL:
            return (length + 7) >> 3
        return length * self.frame_size(data_type)

    def get_array_size(self, r_out: asm.LabelRef, data_type, length: asm.AssemblyExpression):
        assert isinstance(data_type, DataType)
        if isinstance(length, asm.IntLiteral):
            return asm.IntLiteral(self.array_size(data_type, length.data))
        if data_type == DataType.BOOL:
            yield asm.Add(r_out, length, asm.IntLiteral(7))
            yield asm.Asr(r_out, asm.State(r_out), asm.IntLiteral(3))
        else:
            yield asm.Mul(r_out, length, asm.IntLiteral(self.frame_size(data_type)))
        return asm.State(r_out)

    def create_new_stack_array(self, val_type: ConcreteArrayType,
                               origin_bubble, length_bubble,
                               static_size=None):
        if static_size is not None:
            assert static_size == self.array_size(val_type.el_type, length_bubble.value.data)
        else:
            static_size = 0
        bubble: Bubble = length_bubble + origin_bubble
        assert bubble.cur == self.stack
        cur = bubble.cur.add(array_num=1, static_array_size=static_size)
        ref = ArrayRef(val_type, origin=origin_bubble.value, length=length_bubble.value)
        self.allocated_arrays.append(ref)
        self.stack = cur
        assert len(self.allocated_arrays) == self.stack.array_num
        return ValueBubble(bubble.prev, cur, ref)

    def frame_size(self, val_type):
        if isinstance(val_type, ArrayType|ConcreteArrayType):
            return 2 * self.word_size
        assert isinstance(val_type, DataType)
        if val_type.byte_sized:
            return 1
        if val_type == DataType.VOID:
            return 0
        return self.word_size

    def push_value(self, data_type: DataType, value: asm.AssemblyExpression) -> asm.InstrGen[ValueBubble]:
        # push a typed value from AssemblyExpression and set fast_value
        bubble = self.reserve_type(data_type)
        yield from bubble.value.set(value)
        return bubble.with_value(bubble.value, value)

    def pop_value(self, r_out: asm.LabelRef, bubble: ValueBubble) -> asm.InstrGen[asm.AssemblyExpression]:
        assert isinstance(bubble.value, asm.Accessor)
        yield from self.pop(bubble)
        return (yield from bubble.value.get(r_out))

    def pop(self, bubble, *, static=True):
        # We need both static and dynamic deallocation of arrays because
        # we will lose the array reference for the array if it is passed
        # to a function, and must rely on static deallocation based on
        # the known length of the array.
        # However if arrays are dynamically allocated, obviously we need
        # to also dynamically deallocate.  Thankfully you can't create a
        # dynamically allocated array as an unbound expression, so we
        # don't need to worry about losing the references to them.
        # When it is possible, static allocation permits more assertions
        # so it's the default.
        assert self.stack == bubble.cur
        if static:
            diff = bubble.static_array_size
            assert diff == sum(
                self.array_size(array.type.el_type, array.length.data)
                for array in self.allocated_arrays[bubble.prev.array_num:]
            )
            if diff != 0:
                yield asm.Metadata(f'Deallocate {bubble.array_allocations} arrays statically')
                yield asm.Sub(self.ap, asm.State(self.ap), asm.IntLiteral(diff))
        else:
            yield from self.reset_ap(bubble.prev.array_num)
        del self.allocated_arrays[bubble.prev.array_num:]
        self.stack = bubble.prev

    def reset_ap(self, array_idx):
        # Dynamically reset ap to before the array at array_idx in
        # allocated_arrays was allocated.
        # Does NOT modify self.stack / self.allocated_arrays
        cur_arrays = len(self.allocated_arrays)
        if array_idx < cur_arrays:
            num_arrays = cur_arrays - array_idx
            yield asm.Metadata(f'Deallocate {num_arrays} arrays dynamically')
            array = self.allocated_arrays[array_idx]
            yield from array.origin.to(self.ap)
        else:
            assert array_idx == cur_arrays

    @contextlib.contextmanager
    def at_offset(self, offset):
        # Context manager for temporarily setting the stack to an offset
        # Allows use of methods like reserve_type at arbitrary offsets
        prev_stack = self.stack
        new_stack = StackPoint(
            offset=offset, array_num=prev_stack.array_num,
            static_array_size=prev_stack.static_array_size
        )
        self.stack = new_stack

        try:
            yield
        finally:
            # It's ok to leave the offset wherever you like since we're
            # resetting it, but any arrays that may have been allocated
            # do need to be cleaned up.
            assert not Bubble(new_stack, self.stack).has_array, 'stack not cleaned up'
            self.stack = prev_stack
