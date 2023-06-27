import dataclasses as dc

from . import asm
from hidc import ast
from hidc.ast import DataType, ArrayType
from hidc.errors import CodeGenError
import textwrap
import typing as ty


# The stdlib exposes:
#  all_is_win() as all_is_win
#  all_is_broken() as all_is_broken
#  print(const:const byte[]) as print_const_byte_array
#  print(const:state byte[]) as print_state_byte_array
#  print(string) as print_string
#  print(bool) as print_bool
#  print(int) as print_int
# !is_defeat(), !truth_is_defeat(bool), print(byte), and println(...)
# should all be inlined.
stdlib = list(filter(None, textwrap.dedent("""
    all_is_win:
        flag win
        tnt: sleep 0x7f7f
        j tnt
        halt
    all_is_broken:
        flag error
        j tnt
        halt
    index_out_of_bounds:
        flag index_out_of_bounds
        j all_is_broken
        halt
    stack_overflow:
        flag stack_overflow
        j all_is_broken
        halt
    division_by_zero:
        flag division_by_zero
        j all_is_broken
        halt
    

    print_const_byte_array:
        lwso [r0], [fp], -2w
        lwso [r1], [fp], -3w
        j print_string_loop
        halt
    print_string:
        lwso [r0], [fp], -2w
        lwc [r1], [r0]
        add [r0], [r0], 1w
        print_string_loop:
        j print_string_done
        hle [r1], 0
            lbc [r2], [r0]
            yield [r2]
            add [r0], [r0], 1
            sub [r1], [r1], 1
        j print_string_loop
        halt
        print_string_done:
        hgt [r1], 0
        lwso [r0], [fp], -1w
        j [r0]
        halt


    print_state_byte_array:
        lwso [r0], [fp], -2w
        lwso [r1], [fp], -3w
        print_state_byte_array_loop:
        j print_string_done
        hle [r1], 0
            lbs [r2], [r0]
            yield [r2]
            add [r0], [r0], 1
            sub [r1], [r1], 1
        j print_state_byte_array_loop
        halt


    print_bool:
        lbso [r0], [fp], -1w - 1
        j print_bool_is_true
        hne [r0], 0
        yield 'f'
        yield 'a'
        yield 'l'
        yield 's'
        yield 'e'
        lwso [r0], [fp], -1w
        j [r0]
        halt
        print_bool_is_true:
        heq [r0], 0
        yield 't'
        yield 'r'
        yield 'u'
        yield 'e'
        lwso [r0], [fp], -1w
        j [r0]
        halt


    print_int:
        lwso [r2], [fp], -2w
        ; TODO: test for stack overflow, may need a total of 4 words of
        ; stack space: 1 for RA + 3 for the buffer.
        j print_int_pos
        hge [r2], 0
        yield '-'
        sub [r2], 0, [r2]
        print_int_pos:
        hlt [r2], 0

        add [r0], [fp], -1w
        print_int_get_digits:
            mod [r1], [r2], 10
            div [r2], [r2], 10
            add [r1], [r1], '0'
            sub [r0], [r0], 1
            sbs [r0], [r1]
        j print_int_have_digits
        heq [r2], 0
        j print_int_get_digits
        halt
        print_int_have_digits:
        hne [r2], 0

        sub [r1], [fp], [r0]
        sub [r1], [r1], 1w
        j print_state_byte_array_loop
        halt
""").strip('\n').encode('utf-8').split(b'\n')))


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
class CodeGen3:
    program: ast.Program
    word_size: int = 2
    numbered_labels: dict[str, int] = dc.field(init=False, default_factory=dict)
    string_table: dict[bytes, asm.LabelRef] = dc.field(init=False, default_factory=dict)
    indent: int = dc.field(init=False, default=0)
    ap = asm.LabelRef('ap')
    fp = asm.LabelRef('fp')
    r0 = asm.LabelRef('r0')
    r1 = asm.LabelRef('r1')
    r2 = asm.LabelRef('r2')
    # all_is_broken = asm.LabelRef('all_is_broken')
    all_is_win = asm.LabelRef('all_is_win')
    index_out_of_bounds = asm.LabelRef('index_out_of_bounds')
    stack_overflow = asm.LabelRef('stack_overflow')
    division_by_zero = asm.LabelRef('division_by_zero')

    def __post_init__(self):
        if self.word_size < 2:
            raise CodeGenError('Word size must be at least 2 bytes', ())


    def generate(self):
        yield b'%format word ' + str(self.word_size).encode('utf-8')
        yield b'%format output byte'
        yield b'%section code'
        yield from self.asm_lines(self.emit_code())
        yield from stdlib

        yield b'%section state'
        # emit mutable global variables, registers, and stack

        yield b'%section const'
        # emit const global variables, const array literals, and strings
        # (const array literals might just become global variables)

    def emit_code(self):
        # Emit instruction objects for:
        # PUSH all_is_win (actually probably set this up in advance)
        # j main_func (if we set up push all_is_win in advance, we can just emit the main function)
        # halt

        # Emit all functions
        # Comment before each function declaration showing signature
        yield from ()

    def eval_expr(self, expr):
        yield from ()
        # Must implement:
        #  - FuncCall
        #  - StringToByteArray
        #  - Volatile
        #  - VariableLookup (because we need to handle array variables)
        # Probably implemented here:
        #  - ArrayLiteral
        # Probably implemented elsewhere:
        #  - ArrayInitializer
        # For most word expressions: use eval_word_expr_to_reg_or_imm,
        # unless we can avoid putting it in a register

        ...

    # really rolls off the tongue
    def eval_word_expr_to_reg_or_imm(self, r_out:asm.LabelRef, expr)->ty.Generator[asm.Instruction, None, asm.AssemblyExpression]:
        match expr:
            case ast.ByteValue(data):
                return asm.IntLiteral(data, is_byte=True)
            case ast.IntValue(data):
                return asm.IntLiteral(data)
            case ast.BoolValue(data):
                return asm.IntLiteral(int(data))
            case ast.StringValue(data):
                return self.add_string(data)
            case ast.ByteToInt() | ast.IntToByte() | ast.BoolToByte():
                return (yield from self.eval_word_expr_to_reg_or_imm(r_out, expr.expr))
            case ast.IntToBool():
                value = yield from self.eval_word_expr_to_reg_or_imm(r_out, expr.expr)
                if isinstance(value, asm.IntLiteral):
                    # Probably redundant with TC
                    return asm.IntLiteral(int(bool(value.data)))
                bool_normalized = self.add_label('bool_normalized')
                assert value == asm.State(r_out)
                yield asm.Jump(bool_normalized)
                yield asm.Heq(value, asm.IntLiteral(0))
                yield asm.Mov(r_out, asm.IntLiteral(1))
                yield asm.Label(bool_normalized)
                # Needed for halt propagation
                # Unsigned comparisons would be nice...
                yield asm.Hlt(value, asm.IntLiteral(0))
                yield asm.Hgt(value, asm.IntLiteral(1))
                return value
            case ast.BinaryArithmeticOp():
                yield from self.eval_expr(expr.left)
                right = yield from self.eval_word_expr_to_reg_or_imm(self.r1, expr.right)
                yield from self.pop_word_to_reg(self.r0, expr.left.type)
                yield from self.arith_op_reg_arg(type(expr), r_out, asm.State(self.r0), right)
                return asm.State(r_out)
            case ast.Unary():
                value = yield from self.eval_word_expr_to_reg_or_imm(r_out, expr.arg)
                yield from self.un_op_reg_arg(type(expr), r_out, value)
                return asm.State(r_out)
            case ast.BooleanOp():
                yield from self.bool_expr_branch(
                    expr,
                    (asm.Mov(r_out, asm.IntLiteral(1)),),
                    (asm.Mov(r_out, asm.IntLiteral(0)),)
                )
                return asm.State(r_out)
            case ast.FuncCall():
                yield from self.eval_expr(expr)
                yield from self.pop_word_to_reg(r_out, expr.type)
                return asm.State(r_out)
        # Still needed:
        # - LengthLookup:
        #     Must deal with cases like [f(x)].length
        #     Must handle strings
        # - ArrayLookup:
        #     Must deal with cases like [f(x), g(x), h(x)][1]
        #     Must handle strings
        # - VariableLookup:
        #     We cannot handle array variables here
        assert False, f'{expr} not handled as word expression'

    def pop_word_to_reg(self, r_out, data_type):
        assert type(data_type) is DataType, 'array cannot fit in register'
        assert data_type != DataType.VOID, 'should be handled by typechecker'
        yield from ()
        ...

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
            yield from self.eval_expr(expr.left)
            right_arg = yield from self.eval_word_expr_to_reg_or_imm(
                self.r1, expr.right
            )
            yield from self.pop_word_to_reg(self.r0, expr.left.type)
            yield asm.Jump(compare_is_true)
            yield instr(asm.State(self.r0), right_arg)
            yield from if_false
            if not false_end_goto:
                yield from self.goto(compare_end)
            yield asm.Label(compare_is_true)
            yield halt_inversion[instr](asm.State(self.r0), right_arg)
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
            value = yield from self.eval_word_expr_to_reg_or_imm(self.r1, expr)
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

    def arith_op_reg_arg(self, op_type, r_out:asm.LabelRef,
                         arg_left:asm.AssemblyExpression,
                         arg_right:asm.AssemblyExpression):
        if instr := arith_map.get(op_type):
            if op_type in {ast.Div, ast.Mod}:
                yield asm.Jump(div_allowed := self.add_label('div_allowed'))
                yield asm.Hne(arg_right, asm.IntLiteral(0))
                yield self.goto(self.division_by_zero)
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

    def add_label(self, name):
        suffix = self.numbered_labels.get(name, 0)
        self.numbered_labels[name] = suffix + 1
        return asm.LabelRef(f'{name}_{suffix}')

    def add_string(self, data):
        try:
            return self.string_table[data]
        except KeyError:
            label = self.add_label('string')
            self.string_table[data] = label
            return label

    def mark(self, label_ref):
        yield asm.Label(label_ref)
        return label_ref

    def goto(self, addr):
        # unconditional jump
        yield asm.Jump(addr)
        yield asm.Halt()

    def is_goto(self, instructions):
        # TODO: ignore comments?
        return (len(instructions) == 2 and
                isinstance(instructions[0], asm.Jump) and
                isinstance(instructions[1], asm.Halt))

    def asm_lines(self, directives):
        iterator = iter(directives)
        while True:
            try:
                directive = next(iterator)
            except StopIteration as ret:
                return ret.value

            for line in bytes(directive).split(b'\n'):
                return b'    ' * self.indent + line
