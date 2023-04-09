from hidc.parser.grammar import *
from hidc.errors import ParserError
from hidc.parser import parse, expect
from hidc.lexer import SourceCode, Span, Cursor

import typing as ty
from pytest import raises

ctx = BlockContext.FUNC
yctx = BlockContext.YOU
dctx = BlockContext.DEFEAT

def parse_string(string, rule, partial=True):
    return parse(expect(rule), SourceCode.from_string(string), partial)

class Any:
    def __init__(self, cls):
        self.cls = cls

    def __eq__(self, other):
        return isinstance(other, self.cls)

    def __repr__(self):
        return '_'

_: ty.Any = Any(Span | Cursor)

def test_declaration():
    assert parse_string('int x = 42', ps_vdecl(ctx)) == Declaration(
        Variable(const=False, type=DataType(Type.INT), name='x'),
        IntLiteral(42, _), _
    )

    assert parse_string("const byte x = 'B'", ps_vdecl(ctx)) == Declaration(
        Variable(const=True, type=DataType(Type.BYTE), name='x'),
        IntLiteral(ord('B'), _), _
    )

    assert parse_string('bool x[5]', ps_vdecl(ctx)) == Declaration(
        Variable(const=False, type=ArrayType(Type.BOOL), name='x'),
        ArrayInitializer(ArrayType(Type.BOOL), IntLiteral(5, _)), _
    )

    assert parse_string('int[] x = [1, 2, 3]', ps_vdecl(ctx)) == Declaration(
        Variable(const=False, type=ArrayType(Type.INT), name='x'),
        ArrayLiteral((IntLiteral(1, _), IntLiteral(2, _), IntLiteral(3, _)), _), _
    )

    with raises(ParserError): parse_string('int x', ps_vdecl(ctx))
    with raises(ParserError): parse_string('const x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('aint x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int @x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int x = int x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('const int x[5]', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int[] x[5]', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int x[5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int x[int x = 5]', ps_vdecl(ctx))

def test_literal():
    assert parse_string('42', ps_expr(ctx)) == IntLiteral(42, _)
    assert (parse_string('0x42', ps_expr(ctx)) == IntLiteral(0x42, _) ==
            parse_string(r"'\x42'", ps_expr(ctx)))
    assert parse_string('"hi"', ps_expr(ctx)) == StringLiteral(b'hi', _)
    assert parse_string('true', ps_expr(ctx)) == BoolLiteral(True, _)

    assert parse_string('[]', ps_expr(ctx)) == ArrayLiteral((), _)
    assert parse_string('[1]', ps_expr(ctx)) == ArrayLiteral(
        (IntLiteral(1, _),), _
    )

    assert parse_string('[1,2]', ps_expr(ctx)) == ArrayLiteral(
        (IntLiteral(1, _), IntLiteral(2, _)), _
    )

    assert parse_string('[1,2,3]', ps_expr(ctx)) == ArrayLiteral(
        (IntLiteral(1, _), IntLiteral(2, _), IntLiteral(3, _)), _
    )

    with raises(ParserError): parse_string('[1,2', ps_expr(ctx))
    with raises(ParserError): parse_string('[1,,2]', ps_expr(ctx))
    with raises(ParserError): parse_string('[1,2,]', ps_expr(ctx))
    with raises(ParserError): parse_string('[1 2]', ps_expr(ctx))

def test_precedence():
    assert parse_string('1 + 2 + 3', ps_expr(ctx)) == BinaryOp(
        Op.ADD, _,
        BinaryOp(Op.ADD, _, IntLiteral(1, _), IntLiteral(2, _)),
        IntLiteral(3, _)
    )

    assert parse_string('1 + (2 + 3)', ps_expr(ctx)) == BinaryOp(
        Op.ADD, _,
        IntLiteral(1, _),
        BinaryOp(Op.ADD, _, IntLiteral(2, _), IntLiteral(3, _))
    )

    assert parse_string('1 + 2 * 3', ps_expr(ctx)) == BinaryOp(
        Op.ADD, _,
        IntLiteral(1, _),
        BinaryOp(Op.MUL, _, IntLiteral(2, _), IntLiteral(3, _))
    )

    assert parse_string('1 + -2', ps_expr(ctx)) == BinaryOp(
        Op.ADD, _, IntLiteral(1, _),
        UnaryOp(Op.SUB, _, IntLiteral(2, _))
    )

def test_func_flavor():
    assert (
        parse_string('f(x)', ps_expr(ctx)) ==
        parse_string('f(x)', ps_expr(yctx)) ==
        parse_string('f(x)', ps_expr(dctx)) ==
        FuncCall(Ident('f'), (VariableLookup('x', _),), _)
    )

    assert parse_string('!f(x)', ps_expr(dctx)) == FuncCall(
        Ident.defeat('f'), (VariableLookup('x', _),), _
    )

    assert parse_string('@f(x)', ps_expr(yctx)) == FuncCall(
        Ident.you('f'), (VariableLookup('x', _),), _
    )

    with raises(ParserError): parse_string('!f(x)', ps_expr(ctx))
    with raises(ParserError): parse_string('!f(x)', ps_expr(yctx))
    with raises(ParserError): parse_string('@f(x)', ps_expr(ctx))
    with raises(ParserError): parse_string('@f(x)', ps_expr(dctx))


def test_try():
    assert parse_string("""
        void @f() {
            try { preempt {} } undo {}
        }
    """, ps_program()) == Program(
        var_decls=(),
        func_decls=(FuncDeclaration(
            _, DataType(Type.VOID),
            FuncSignature(Ident.you('f'), ()), CodeBlock((
                TryBlock(
                    _, CodeBlock((
                        PreemptBlock(_, CodeBlock.empty(_)),
                    ), _),
                    UndoBlock(_, CodeBlock.empty(_))),
            ), _)
        ),)
    )

def test_out_of_place():
    with raises(ParserError): parse_string("""
        void f() {
            try { preempt {} } undo {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        void !f() {
            try { preempt {} } undo {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        void @f() {
            try { try {} undo {} } undo {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        void @f() {
            preempt {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        void f() {
            break;
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        void f() {
            continue;
        }
    """, ps_program())

def test_return():
    assert parse_string("""
        int f(int x) {
            return x + 1;
        }
    """, ps_program()) == Program(
        var_decls=(),
        func_decls=(FuncDeclaration(
            _, DataType(Type.INT), FuncSignature(
                Ident('f'),
                (Variable(const=False, type=DataType(Type.INT), name='x'),)
            ),
            CodeBlock((ReturnStatement(
                _, BinaryOp(
                    Op.ADD, _,
                    VariableLookup('x', _), IntLiteral(1, _)
                )
            ),), _)
        ),)
    )

def test_for():
    assert parse_string("""
        for (int i = 0; i < arr.length; i += 1) {
            arr[i] = 0;
        }
    """, ps_block(ctx)) == CodeBlock((
        Declaration(
            Variable(const=False, type=DataType(Type.INT), name='i'),
            IntLiteral(0, _), _
        ),
        LoopBlock(
            _, CodeBlock((
                Assignment(
                    ArrayLookup(VariableLookup('arr', _), VariableLookup('i', _), _),
                    IntLiteral(0, _)
                ),
            ),_),
            BinaryOp(Op.LT, _,
                     VariableLookup('i', _),
                     LengthLookup(VariableLookup('arr', _), _)),
            CodeBlock((
                IncAssignment(VariableLookup('i', _), IntLiteral(1, _), Op.ADD),
            ), _)
        )
    ), _)

    with raises(ParserError): parse_string("""
        for (int i = 0; i < arr.length; i += 1)
    """, ps_block(ctx))

    with raises(ParserError): parse_string("""
        for (int i = 0; i < arr.length; int i = 1) {
            arr[i] = 0;
        }
    """, ps_block(ctx))

    with raises(ParserError): parse_string("""
        for (int i = 0; i += 1; i += 1) {
            arr[i] = 0;
        }
    """, ps_block(ctx))

    with raises(ParserError): parse_string("""
        for (return; i < arr.length; i += 1) {
            arr[i] = 0;
        }
    """, ps_block(ctx))

def test_if():
    assert parse_string("""
        if (x == 1 or y == 2) {
            println("potato");
        }
    """, ps_block(ctx)) == IfBlock(
        _, CodeBlock((
            FuncCall(
                Ident('println'),
                (StringLiteral(b'potato', _),), _
            ),
        ), _),
        BinaryOp(
            Op.OR, _,
            BinaryOp(Op.EQ, _, VariableLookup('x', _), IntLiteral(1, _)),
            BinaryOp(Op.EQ, _, VariableLookup('y', _), IntLiteral(2, _))
        ),
        CodeBlock.empty(_)
    )

def test_else():
    assert parse_string("""
        if (a) {
            1;
        } else if (b) {
            2;
        } else if (c) {
            3;
        } else {
            4;
        }
    """, ps_block(ctx)) == IfBlock(
        _, CodeBlock((IntLiteral(1, _),),_),
        VariableLookup('a', _),
        IfBlock(
            _, CodeBlock((IntLiteral(2, _),), _),
            VariableLookup('b', _),
            IfBlock(
                _, CodeBlock((IntLiteral(3, _),), _),
                VariableLookup('c', _),
                CodeBlock((IntLiteral(4, _),), _)
            )
        )
    )

def test_while():
    assert parse_string("""
        while (true) {
            if (false) {
                continue;
            }
            break;
        }
    """, ps_block(ctx)) == LoopBlock(
        _, CodeBlock((
            IfBlock(
                _, CodeBlock((ContinueStatement(_),), _),
                BoolLiteral(False,_),
                CodeBlock.empty(_)
            ),
            BreakStatement(_),
        ), _),
        BoolLiteral(True, _), CodeBlock.empty(_)
    )

def test_program():
    assert parse_string("""
        // Nothing here
    """, ps_program()) == Program((), ())

    assert parse_string("""
        int x = -5;
        void @is_you() {}
    """, ps_program()) == Program(
        var_decls=(Declaration(
            Variable(const=False, type=DataType(Type.INT), name='x'),
            UnaryOp(Op.SUB, _, IntLiteral(5, _)), _
        ),),
        func_decls=(FuncDeclaration(
            _, DataType(Type.VOID),
            FuncSignature(Ident.you('is_you'), ()),
            CodeBlock.empty(_)
        ),)
    )

    with raises(ParserError): parse_string('int x = f(5);', ps_program())
    with raises(ParserError): parse_string('int x = @f(5);', ps_program())
    with raises(ParserError): parse_string('int x = !f(5);', ps_program())

def test_hash_eq():
    test = """
        const byte[] arr = ['a', 'b', 'c'];
        int x[42];
        
        void @is_you() {
            x[0] = -arr[arr.length - 1] + 1;
            try {
                for (int i = 0; i < arr.length; i += 1) {
                    if (true) {
                        continue;
                    } else {
                        preempt {
                            break;
                        }
                    }
                }
                !is_defeat();
            } undo {
                return;
            }
        }
    """

    assert {parse_string(test, ps_program())} == {parse_string(test, ps_program())}

def test_weird_int_again():
    # Follow-up from lexer tests
    with raises(ParserError): parse_string('int x = 0_;', ps_program())
    with raises(ParserError): parse_string('int x = 1__000;', ps_program())
    with raises(ParserError): parse_string('int x = 0b;', ps_program())
    with raises(ParserError): parse_string('int x = 0b_;', ps_program())
    with raises(ParserError): parse_string('int x = 0b_0;', ps_program())
    with raises(ParserError): parse_string('int x = 0b0_;', ps_program())

def test_enum_sanity_again():
    assert BlockContext.TRY in (BlockContext.TRY | BlockContext.LOOP)
    assert BlockContext.YOU.flavors == frozenset({Flavor.YOU, Flavor.NONE})
    with raises(ValueError): BlockContext.YOU | BlockContext.DEFEAT
    with raises(ValueError): BlockContext.YOU | BlockContext.TRY
