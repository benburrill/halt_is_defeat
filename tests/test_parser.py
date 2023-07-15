from hidc.ast import *
from hidc.parser.grammar import *
from hidc.parser.rules import expect
from hidc.parser import parse, ParserError
from hidc.lexer import SourceCode, Span, Cursor

import typing as ty
from pytest import raises

ctx = BlockContext.FUNC
yctx = BlockContext.YOU
dctx = BlockContext.DEFEAT

def parse_string(string, rule, partial=True):
    return parse(SourceCode.from_string(string), expect(rule), partial)

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
        Variable('x', DataType.INT, const=False),
        IntValue(42, _), _
    )

    assert parse_string("const byte x = 'B'", ps_vdecl(ctx)) == Declaration(
        Variable('x', DataType.BYTE, const=True),
        ByteValue(ord('B'), _), _
    )

    assert parse_string('bool x[5]', ps_vdecl(ctx)) == Declaration(
        Variable('x', ArrayType(DataType.BOOL, const=False), const=True),
        ArrayInitializer(ArrayType(DataType.BOOL, const=False), IntValue(5, _)), _
    )

    assert parse_string('int[] x = [1, 2, 3]', ps_vdecl(ctx)) == Declaration(
        Variable('x', ArrayType(DataType.INT, const=False), const=True),
        ArrayLiteral((IntValue(1, _), IntValue(2, _), IntValue(3, _)), _), _
    )

    with raises(ParserError): parse_string('int x', ps_vdecl(ctx))
    with raises(ParserError): parse_string('const x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('aint x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int @x = 5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int x = int x = 5', ps_vdecl(ctx))
    # with raises(ParserError): parse_string('const int x[5]', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int[] x[5]', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int x[5', ps_vdecl(ctx))
    with raises(ParserError): parse_string('int x[int x = 5]', ps_vdecl(ctx))

def test_literal():
    assert parse_string('42', ps_expr(ctx)) == IntValue(42, _)
    assert parse_string(r"'\x42'", ps_expr(ctx)) == ByteValue(0x42, _)
    assert parse_string('"hi"', ps_expr(ctx)) == StringValue(b'hi', _)
    assert parse_string('true', ps_expr(ctx)) == BoolValue(True, _)

    assert parse_string('[]', ps_expr(ctx)) == ArrayLiteral((), _)
    assert parse_string('[1]', ps_expr(ctx)) == ArrayLiteral(
        (IntValue(1, _),), _
    )

    assert parse_string('[1,2]', ps_expr(ctx)) == ArrayLiteral(
        (IntValue(1, _), IntValue(2, _)), _
    )

    assert parse_string('[1,2,3]', ps_expr(ctx)) == ArrayLiteral(
        (IntValue(1, _), IntValue(2, _), IntValue(3, _)), _
    )

    with raises(ParserError): parse_string('[1,2', ps_expr(ctx))
    with raises(ParserError): parse_string('[1,,2]', ps_expr(ctx))
    with raises(ParserError): parse_string('[1,2,]', ps_expr(ctx))
    with raises(ParserError): parse_string('[1 2]', ps_expr(ctx))

def test_precedence():
    assert parse_string('1 + 2 + 3', ps_expr(ctx)) == Add(
        _, Add(_, IntValue(1, _), IntValue(2, _)),
        IntValue(3, _)
    )

    assert parse_string('1 + (2 + 3)', ps_expr(ctx)) == Add(
        _, IntValue(1, _),
        Add(_, IntValue(2, _), IntValue(3, _))
    )

    assert parse_string('1 + 2 * 3', ps_expr(ctx)) == Add(
        _, IntValue(1, _),
        Mul(_, IntValue(2, _), IntValue(3, _))
    )

    assert parse_string('1 + -2', ps_expr(ctx)) == Add(
        _, IntValue(1, _),
        Neg(_, IntValue(2, _))
    )

def test_func_flavor():
    assert (
        parse_string('f(x)', ps_expr(ctx)) ==
        parse_string('f(x)', ps_expr(yctx)) ==
        parse_string('f(x)', ps_expr(dctx)) ==
        FuncCall(Ident('f'), (VariableLookup(UnresolvedName('x'), _),), _)
    )

    assert parse_string('!f(x)', ps_expr(dctx)) == FuncCall(
        Ident.defeat('f'), (VariableLookup(UnresolvedName('x'), _),), _
    )

    assert parse_string('@f(x)', ps_expr(yctx)) == FuncCall(
        Ident.you('f'), (VariableLookup(UnresolvedName('x'), _),), _
    )

    with raises(ParserError): parse_string('!f(x)', ps_expr(ctx))
    with raises(ParserError): parse_string('!f(x)', ps_expr(yctx))
    with raises(ParserError): parse_string('@f(x)', ps_expr(ctx))
    with raises(ParserError): parse_string('@f(x)', ps_expr(dctx))


def test_misc_expr():
    assert parse_string('arr[0].length', ps_expr(ctx)) == LengthLookup(
        ArrayLookup(
            VariableLookup(UnresolvedName('arr'), _),
            IntValue(0, _), _
        ), _
    )

    # Multidimensional arrays will probably never be supported, but
    # should parse regardless.
    assert parse_string('arr[0][1][2]', ps_expr(ctx)) == ArrayLookup(
        ArrayLookup(
            ArrayLookup(
                VariableLookup(UnresolvedName('arr'), _),
                IntValue(0, _), _
            ), IntValue(1, _), _
        ), IntValue(2, _), _
    )

    assert parse_string('[2][1][0]', ps_expr(ctx)) == ArrayLookup(
        ArrayLookup(
            ArrayLiteral((IntValue(2, _),), _),
            IntValue(1, _), _
        ), IntValue(0, _), _
    )

def test_try():
    assert parse_string("""
        try { preempt {} } undo {}
    """, ps_block(yctx)) == TryBlock(
        _, CodeBlock((
            PreemptBlock(_, CodeBlock.empty(_)),
        ), _),
        UndoBlock(_, CodeBlock.empty(_))
    ) == parse_string("""
        empty @f() {
            try { preempt {} } undo {}
        }
    """, ps_program()).func_decls[0].body.stmts[0]

    with raises(ParserError): parse_string("""
        try { preempt {} } 
    """, ps_block(yctx))

    with raises(ParserError): parse_string("""
        try { preempt {} } undo
    """, ps_block(yctx))

def test_out_of_place():
    with raises(ParserError): parse_string("""
        empty f() {
            try { preempt {} } undo {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        empty !f() {
            try { preempt {} } undo {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        empty @f() {
            try { try {} undo {} } undo {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        empty @f() {
            preempt {}
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        empty f() {
            break;
        }
    """, ps_program())

    with raises(ParserError): parse_string("""
        empty f() {
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
            _, DataType.INT, Ident('f'), (
                Parameter(Variable('x', DataType.INT, const=False),_),
            ),
            CodeBlock((ReturnStatement(
                _, Add(_, VariableLookup(UnresolvedName('x'), _), IntValue(1, _))
            ),), _)
        ),)
    )

def test_for():
    # We cannot use LoopBlock.for_loop, since it expects an actual span
    assert parse_string("""
        for (int i = 0; i < arr.length; i += 1) {
            arr[i] = 0;
        }
    """, ps_block(ctx)) == CodeBlock((
        Declaration(
            Variable('i', DataType.INT, const=False),
            IntValue(0, _), _
        ),
        LoopBlock(
            start=_,
            cond=Lt(
                _, VariableLookup(UnresolvedName('i'), _),
                LengthLookup(VariableLookup(UnresolvedName('arr'), _), _)
            ),
            body=CodeBlock((
                Assignment(
                    ArrayLookup(
                        VariableLookup(UnresolvedName('arr'), _),
                        VariableLookup(UnresolvedName('i'), _), _
                    ),
                    IntValue(0, _)
                ),
            ),_),
            cont=CodeBlock((
                IncAssignment(
                    VariableLookup(UnresolvedName('i'), _),
                    IntValue(1, _),
                    Add, _
                ),
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
            writeln("potato");
        }
    """, ps_block(ctx)) == IfBlock(
        start=_,
        cond=Or(_,
            Eq(_, VariableLookup(UnresolvedName('x'), _), IntValue(1, _)),
            Eq(_, VariableLookup(UnresolvedName('y'), _), IntValue(2, _))
        ),
        body=CodeBlock((
            FuncCall(
                Ident('writeln'),
                (StringValue(b'potato', _),), _
            ),
        ), _),
        else_block=CodeBlock.empty(_)
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
        start=_,
        cond=VariableLookup(UnresolvedName('a'), _),
        body=CodeBlock((IntValue(1, _),), _),
        else_block=IfBlock(
            start=_,
            cond=VariableLookup(UnresolvedName('b'), _),
            body=CodeBlock((IntValue(2, _),), _),
            else_block=IfBlock(
                start=_,
                cond=VariableLookup(UnresolvedName('c'), _),
                body=CodeBlock((IntValue(3, _),), _),
                else_block=CodeBlock((IntValue(4, _),), _)
            )
        )
    )

    with raises(ParserError): parse_string("""
        if (a) {
            1;
        } else
    """, ps_block(ctx))

def test_while():
    assert parse_string("""
        while (true) {
            if (false) {
                continue;
            }
            break;
        }
    """, ps_block(ctx)) == LoopBlock.while_loop(
        start=_,
        cond=BoolValue(True, _),
        body=CodeBlock((
            IfBlock(
                start=_,
                cond=BoolValue(False, _),
                body=CodeBlock((ContinueStatement(_),), _),
                else_block=CodeBlock.empty(_)
            ),
            BreakStatement(_),
        ), _)
    )

def test_block():
    assert parse_string('{}', ps_block(ctx)) == CodeBlock((), _)
    assert parse_string('{ ; }', ps_block(ctx)) == CodeBlock((), _)
    assert parse_string('{int x = 0;}', ps_block(ctx)) == CodeBlock((
        Declaration(
            Variable('x', DataType.INT, const=False),
            IntValue(0, _), _
        ),
    ), _)

    with raises(ParserError): assert parse_string('{int x = 0}', ps_block(ctx))

def test_program():
    assert parse_string('// Nothing here', ps_program()) == Program((), ())
    assert parse_string(';', ps_program()) == Program((), ())

    assert parse_string("""
        int x = -5;
        empty @is_you() {}
    """, ps_program()) == Program(
        var_decls=(Declaration(
            Variable('x', DataType.INT, const=False),
            Neg(_, IntValue(5, _)), _
        ),),
        func_decls=(FuncDeclaration(
            _, DataType.EMPTY, Ident.you('is_you'), (),
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
        
        empty @is_you() {
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
