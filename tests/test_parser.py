from hidc.lexer import lex
from hidc.parser import Parser
from hidc.grammar import *
from hidc.tokens import *
from hidc.ast import *

from hidc.utils.lazylist import lazy_list
from hidc.utils.scanner import SourceCode, Span, Cursor
from hidc.errors import ParserError

from pytest import raises

def ps(string):
    return Parser(lazy_list(lex(SourceCode.from_string(string))))

class Anywhere:
    def __eq__(self, other):
        if isinstance(other, Span | Cursor):
            return True
        return NotImplemented

def test_declaration():
    assert ps('int x = 42').parse(syn_vdecl()) == Declaration(
        Variable(const=False, type=DataType(TypeToken.INT), name='x'),
        IntLiteral(42, Anywhere()), Anywhere()
    )

    assert ps("const byte x = 'B'").parse(syn_vdecl()) == Declaration(
        Variable(const=True, type=DataType(TypeToken.BYTE), name='x'),
        IntLiteral(ord('B'), Anywhere()), Anywhere()
    )

    assert ps('bool x[5]').parse(syn_vdecl()) == Declaration(
        Variable(const=False, type=ArrayType(TypeToken.BOOL), name='x'),
        ArrayInitializer(ArrayType(TypeToken.BOOL), IntLiteral(5, Anywhere())),
        Anywhere()
    )

    with raises(ParserError): ps('int x = int x = 5').expect(syn_vdecl())
    with raises(ParserError): ps('const int x[5]').expect(syn_vdecl())
    with raises(ParserError): ps('int[] x[5]').expect(syn_vdecl())
    with raises(ParserError): ps('int x[5').expect(syn_vdecl())
    with raises(ParserError): ps('int x[int x = 5]').expect(syn_vdecl())
