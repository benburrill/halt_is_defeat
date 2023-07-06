from . import asm
from . import stdlib
from hidc import ast
from hidc.ast import DataType, ArrayType
from .symbols import ConcreteType, ConcreteArrayType, ConcreteSignature, AccessMode
from hidc.errors import CodeGenError, InternalCompilerError

from collections import deque
import dataclasses as dc
import typing as ty


@dc.dataclass(frozen=True)
class ArrayRef:
    type: ConcreteArrayType
    origin: asm.Accessor
    length: asm.Accessor

    @property
    def section(self):
        return self.type.access.section


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
    asm.Hle: asm.Hgt, asm.Hge: asm.Hlt
}


@dc.dataclass
class CodeGen:
    func_decls: dict[ast.Ident, dict[tuple[ast.Type, ...], ast.FuncDeclaration]]
    global_decls: dict[str, ast.Declaration]
    word_size: int = 2
    stack_size: int = 50 # in words

    numbered_labels: dict[str, int] = dc.field(init=False, default_factory=dict)
    string_labels: dict[bytes, asm.LabelRef] = dc.field(init=False, default_factory=dict)
    stack: StackPoint = dc.field(init=False, default=StackPoint())
    allocated_arrays: list[ArrayRef] = dc.field(init=False, default_factory=list)
    func_queue: deque[ConcreteSignature] = dc.field(init=False, default_factory=deque)
    func_labels: dict[ConcreteSignature, asm.LabelRef] = dc.field(init=False, default_factory=dict)
    func_table: dict[ConcreteSignature, list[asm.Instruction]] = dc.field(init=False, default_factory=dict)

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

        if not self.func_decls.get(ast.Ident.you('is_you'), {}).get(()):
            raise CodeGenError('Missing @is_you()', ())

        self.func_labels.update(stdlib.stdlib_funcs)
        self.label_for_func(ConcreteSignature(ast.Ident.you('is_you'), ()))

    @classmethod
    def from_program(cls, program: ast.Program, word_size=2):
        func_decls = {}
        var_decls = {}
        for func_decl in program.func_decls:
            param_types = tuple(p.type for p in func_decl.params)
            func_decls.setdefault(func_decl.name, {})[param_types] = func_decl
        for var_decl in program.var_decls:
            var_decls[var_decl.var.name] = var_decl
        return cls(func_decls, var_decls, word_size)

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
        # emit mutable global variables

        yield b'%section const'
        # emit const global variables, const array literals, and strings
        # (const array literals might just become global variables)

        for string, label in self.string_labels.items():
            yield from asm.Label(label).lines()
            yield from asm.WordDirective(asm.IntLiteral(len(string))).lines()
            yield from asm.AsciiDirective(string).lines()
        yield b'%section code'
        for code in self.func_table.values():
            yield from asm.lines(code)
        yield from asm.Metadata('Standard library routines:').lines()
        yield from stdlib.stdlib_lines

    def make_funcs(self):
        while self.func_queue:
            csig = self.func_queue.pop()
            abstract_types = tuple(
                t if isinstance(t, DataType)
                else ArrayType(t.el_type, t.access != AccessMode.RW)
                for t in csig.arg_types
            )

            self.func_table[csig] = list(
                self.gen_func(csig, self.func_decls[csig.name][abstract_types])
            )

    def gen_func(self, csig: ConcreteSignature, func: ast.FuncDeclaration):
        assert self.stack == StackPoint(0)

        # Reserve space for RA
        bubble = self.reserve_word()
        for arg_type in csig.arg_types:
            # TODO: actually put them somewhere
            bubble += self.reserve_type(arg_type)

        yield asm.Metadata(f'Function {func.signature}:')
        yield asm.Label(self.func_labels[csig])
        yield from self.gen_block(func.body)

        cleanup_args = list(self.pop(bubble))
        assert not cleanup_args, 'Should not require any instructions to be clean up arguments'
        assert self.stack == StackPoint(0)

    def gen_block(self, block: ast.Block):
        match block:
            case ast.CodeBlock():
                yield asm.Metadata(add_indent=1)
                yield from self.gen_stmts(block.stmts)
                yield asm.Metadata(add_indent=-1)
            case ast.IfBlock():
                yield asm.Metadata('if block')
                raise NotImplementedError
            case ast.LoopBlock():
                yield asm.Metadata('loop block')
                raise NotImplementedError
            case ast.TryBlock():
                yield asm.Metadata('try block')
                raise NotImplementedError
            case ast.UndoBlock():
                yield asm.Metadata('undo block')
                raise NotImplementedError
            case ast.PreemptBlock():
                yield asm.Metadata('preempt block')
                raise NotImplementedError
            case unknown:
                raise InternalCompilerError(f'Unrecognized block {unknown}')

    def gen_stmts(self, stmts):
        for stmt in stmts:
            yield asm.Metadata(f'Statement @ {stmt.span}: {stmt.__class__.__name__}')
            match stmt:
                case ast.Expression():
                    bubble = yield from self.eval_expr(self.r1, stmt, keep=False)
                    yield from self.pop(bubble)
                case ast.Block():
                    yield from self.gen_block(stmt)
                case ast.Declaration():
                    raise NotImplementedError
                case ast.Assignment():
                    raise NotImplementedError
                case ast.ReturnStatement():
                    if stmt.value is not None:
                        raise NotImplementedError
                    yield asm.Lwso(self.r1, asm.State(self.fp), asm.WordOffset(-1))
                    yield from self.goto(asm.State(self.r1))
                    # TODO: we need to pop or something IDK
                case ast.BreakStatement():
                    raise NotImplementedError
                case ast.ContinueStatement():
                    raise NotImplementedError
                case unknown:
                    raise InternalCompilerError(f'Unrecognized statement {unknown}')

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
            case ast.BoolToByte() | ast.ByteToInt() | ast.Volatile():
                return (yield from self.eval_expr(r_out, expr, keep))
            case ast.IntToByte():
                # In many cases, f(word) = f(byte) (mod 256)
                # but we can't rely on that in general -- add option?
                bubble = yield from self.eval_expr(r_out, expr, keep)
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

                # Unsigned comparisons would be nice...  They would let
                # us skip the Mov if it's already 1 and also have only a
                # single halt propagation instruction.
                yield asm.Jump(bool_normalized)
                yield asm.Heq(asm.State(r_out), asm.IntLiteral(0))
                yield asm.Mov(r_out, asm.IntLiteral(1))
                yield asm.Label(bool_normalized)
                yield asm.Hlt(asm.State(r_out), asm.IntLiteral(0))
                yield asm.Hgt(asm.State(r_out), asm.IntLiteral(1))
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
                ...
                raise NotImplementedError
            case ast.FuncCall():
                return (yield from self.eval_func_call(expr.type, expr.func, expr.args))
            case ast.ArrayLiteral():
                # If const and all elements static: new global, vac Bubble
                # Else: evaluate all elements, +static_array_size, +offset
                ...
                raise NotImplementedError
            case ast.ArrayInitializer():
                # TODO: stack overflow and negative length detection
                length_bubble = yield from self.push_expr(r_out, expr.length)
                length = yield from length_bubble.get_fast(r_out)
                origin_bubble = self.reserve_word()
                yield from origin_bubble.value.set(asm.State(self.ap))
                size = yield from self.get_array_size(r_out, expr.type.el_type, length)
                yield asm.Add(self.ap, asm.State(self.ap), size)
                # It should always be RW, but we'll leave it up to the
                # typechecker to make such decisions.
                access_mode = AccessMode.R if expr.type.const else AccessMode.RW
                return (length_bubble + origin_bubble).with_value(ArrayRef(
                    ConcreteArrayType(expr.type.el_type, access_mode),
                    origin_bubble.value, length_bubble.value
                ))
            case ast.ArrayLookup():
                ...
                raise NotImplementedError
            case ast.LengthLookup():
                # TODO: in the case that the bubble is on the stack, can
                #  we just pop bubble and reserve word?
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
        yield asm.Sub(self.fp, asm.State(self.fp), asm.IntLiteral(offset))
        yield from self.goto(label)
        yield asm.Label(end_call)
        yield asm.Add(self.fp, asm.State(self.fp), asm.IntLiteral(offset))
        yield from self.pop(bubble)
        return self.reserve_type(ret_type)

    def get_checked_index(self, r_idx_out: asm.LabelRef, idx_arg: asm.AssemblyExpression,
                          length_arg: asm.AssemblyExpression) -> asm.InstrGen[asm.AssemblyExpression]:
        # idx_arg should match r_idx_out to avoid unnecessary copies
        index_positive = self.add_label('index_positive')
        index_in_bounds = self.add_label('index_in_bounds')

        if isinstance(idx_arg, asm.IntLiteral):
            # Fast track for IntLiteral (otherwise we'd need to copy it
            # for halt propagation reasons, below)
            if idx_arg.data < 0:
                # We could do extra optimizations in the case length is
                # also immediate, but it's not necessary.
                yield asm.Add(r_idx_out, length_arg, idx_arg)
                idx_arg = asm.State(r_idx_out)
        else:
            # Halt propagation will fail if idx_arg != State(r_idx_out)
            # since idx_arg wouldn't get changed by the Add.
            # So we must use idx_arg.to() to ensure it's in r_idx_out
            yield from idx_arg.to(r_idx_out)
            idx_arg = asm.State(r_idx_out)
            yield asm.Jump(index_positive)
            yield asm.Hge(idx_arg, asm.IntLiteral(0))
            yield asm.Add(r_idx_out, length_arg, idx_arg)
            yield asm.Label(index_positive)
            yield asm.Hlt(idx_arg, asm.IntLiteral(0))

        yield asm.Jump(index_in_bounds)
        yield asm.Hlt(idx_arg, length_arg)
        yield from self.goto(stdlib.index_out_of_bounds)
        yield asm.Label(index_in_bounds)
        return idx_arg

    # def get_array_reference(self, expr, *, copy=False):
    #     # Returns tuple: Bubble, array ref
    #     yield from ()
    #     return ...

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
                yield self.goto(stdlib.division_by_zero)
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

    def lookup_idx_to_reg(self, r_out, src_expr, idx_expr):
        if src_expr.type == DataType.STRING:
            bubble = yield from self.eval_expr(self.r2, src_expr, keep=True)
            index = yield from self.get_expr_value(self.r1, idx_expr)
            source = yield from self.pop_value(self.r2, bubble)
            yield asm.Lwc(self.r0, source)
            index = yield from self.get_checked_index(self.r1, index, asm.State(self.r0))

            # If either source_arg or idx_arg is immediate, this
            # add could be combined into the lbco if I wanted.
            # But I'd need to be aware that source_arg might be
            # a LabelRef, as in the case of "hello"[0]
            yield asm.Add(self.r1, index, asm.IntLiteral(self.word_size))
            yield asm.Lbco(r_out, source, asm.State(self.r1))
            return asm.State(r_out)
        elif isinstance(src_expr.type, ArrayType):
            # Unlike for strings it is safe to leave an array reference
            # in a bubble, we do not need to keep it.  However, the plan
            # is to write eval_expr in a way so that it knows that and
            # does not copy such references to the stack.

            bubble = yield from self.eval_expr(self.r2, src_expr, keep=True)
            index = yield from self.get_expr_value(self.r1, idx_expr)
            length = yield from bubble.value.length.get(self.r0)
            index = yield from self.get_checked_index(self.r1, index, length)
            origin = yield from bubble.value.origin.get(self.r0)

            section = bubble.value.section
            if src_expr.type.el_type == DataType.BOOL:
                yield asm.And(self.r2, origin, asm.IntLiteral(7))
                yield asm.Asr(self.r1, origin, asm.IntLiteral(3))
                yield section.lbo(self.r1, origin, asm.State(self.r1))
                yield asm.Asr(self.r1, asm.State(self.r1), asm.State(self.r2))
                yield asm.And(r_out, asm.State(self.r1), asm.IntLiteral(1))
            elif src_expr.type.el_type == DataType.BYTE:
                yield section.lbo(r_out, origin, index)
            else:
                yield asm.Mul(self.r1, index, asm.IntLiteral(self.word_size))
                yield section.lwo(r_out, origin, asm.State(self.r1))

            yield from self.pop(bubble)
            return asm.State(r_out)
        else:
            assert False

    # def array_assignment(self, arr_expr, idx_expr, rhs_expr, bin_op=None):
    #     assert isinstance(arr_expr.type, ArrayType)
    #     bubble, ref = yield from self.get_array_reference(arr_expr)
    #     idx_arg = yield from self.eval_word_expr_to_reg_or_imm(self.r1, idx_expr)
    #     length_arg = ref.use_length(self.r0)
    #     idx_arg = yield from self.checked_index_to_reg_or_imm(self.r1, idx_arg, length_arg)
    #     if arr_expr.type.el_type == DataType.BOOL:
    #         # No incremental assignments currently exist for BOOL
    #         assert bin_op is None
    #         ...
    #     else:
    #         yield from self.old_push_value(...)
    #
    #     yield from self.pop(bubble)

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
        # TODO: looks like in stdlib I assumed length is at
        #  lower address (top of stack), but in other stuff I
        #  had other way around?  Which should I go with?
        #  Here is assuming we go back to the convention where
        #  address is at low address, but that would require
        #  changing stdlib.
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
                yield asm.Sub(self.ap, asm.State(self.ap), asm.IntLiteral(diff))
        else:
            yield from self.reset_ap(bubble.prev.array_num)
        del self.allocated_arrays[bubble.prev.array_num:]
        self.stack = bubble.prev

    def reset_ap(self, array_idx):
        # Dynamically reset ap to before the array at array_idx in
        # allocated_arrays was allocated.
        if array_idx < len(self.allocated_arrays):
            array = self.allocated_arrays[array_idx]
            yield from array.origin.to(self.ap)
        else:
            assert array_idx == len(self.allocated_arrays)
