from hidc.ast import *
from hidc.grammar import *
from hidc.tokens import *
from hidc.parser import parse, expect

from hidc.utils.scanner import SourceCode, Span, Cursor
from hidc.errors import ParserError

from pytest import raises

ctx = BlockContext.FUNC
yctx = BlockContext.YOU
dctx = BlockContext.DEFEAT
tctx = BlockContext.TRY

def parse_string(string, rule, partial=True):
    return parse(expect(rule), SourceCode.from_string(string), partial)

class Anywhere:
    def __eq__(self, other):
        if isinstance(other, Span | Cursor):
            return True
        return NotImplemented

_ = Anywhere()

def test_declaration():
    assert parse_string('int x = 42', ps_vdecl(ps_expr(ctx))) == Declaration(
        Variable(const=False, type=DataType(TypeToken.INT), name='x'),
        IntLiteral(42, _), _
    )

    assert parse_string("const byte x = 'B'", ps_vdecl(ps_expr(ctx))) == Declaration(
        Variable(const=True, type=DataType(TypeToken.BYTE), name='x'),
        IntLiteral(ord('B'), _), _
    )

    assert parse_string('bool x[5]', ps_vdecl(ps_expr(ctx))) == Declaration(
        Variable(const=False, type=ArrayType(TypeToken.BOOL), name='x'),
        ArrayInitializer(ArrayType(TypeToken.BOOL), IntLiteral(5, _)), _
    )

    assert parse_string('int[] x = [1, 2, 3]', ps_vdecl(ps_expr(ctx))) == Declaration(
        Variable(const=False, type=ArrayType(TypeToken.INT), name='x'),
        ArrayLiteral((IntLiteral(1, _), IntLiteral(2, _), IntLiteral(3, _)), _), _
    )

    with raises(ParserError): parse_string('int x', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('const x = 5', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('aint x = 5', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('int @x = 5', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('int x = int x = 5', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('const int x[5]', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('int[] x[5]', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('int x[5', ps_vdecl(ps_expr(ctx)))
    with raises(ParserError): parse_string('int x[int x = 5]', ps_vdecl(ps_expr(ctx)))

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
