from hidc.parser import parse, expect
from hidc.grammar import *
from hidc.tokens import *
from hidc.ast import *

from hidc.utils.scanner import SourceCode, Span, Cursor
from hidc.errors import ParserError

from pytest import raises

def parse_string(string, rule, partial=True):
    return parse(expect(rule), SourceCode.from_string(string), partial)

class Anywhere:
    def __eq__(self, other):
        if isinstance(other, Span | Cursor):
            return True
        return NotImplemented

def test_declaration():
    assert parse_string('int x = 42', ps_vdecl()) == Declaration(
        Variable(const=False, type=DataType(TypeToken.INT), name='x'),
        IntLiteral(42, Anywhere()), Anywhere()
    )

    assert parse_string("const byte x = 'B'", ps_vdecl()) == Declaration(
        Variable(const=True, type=DataType(TypeToken.BYTE), name='x'),
        IntLiteral(ord('B'), Anywhere()), Anywhere()
    )

    assert parse_string('bool x[5]', ps_vdecl()) == Declaration(
        Variable(const=False, type=ArrayType(TypeToken.BOOL), name='x'),
        ArrayInitializer(ArrayType(TypeToken.BOOL), IntLiteral(5, Anywhere())),
        Anywhere()
    )

    with raises(ParserError): parse_string('int x', ps_vdecl())
    with raises(ParserError): parse_string('const x = 5', ps_vdecl())
    with raises(ParserError): parse_string('aint x = 5', ps_vdecl())
    with raises(ParserError): parse_string('int @x = 5', ps_vdecl())
    with raises(ParserError): parse_string('int x = int x = 5', ps_vdecl())
    with raises(ParserError): parse_string('const int x[5]', ps_vdecl())
    with raises(ParserError): parse_string('int[] x[5]', ps_vdecl())
    with raises(ParserError): parse_string('int x[5', ps_vdecl())
    with raises(ParserError): parse_string('int x[int x = 5]', ps_vdecl())
