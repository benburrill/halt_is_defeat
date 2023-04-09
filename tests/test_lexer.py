from hidc.lexer.tokens import *
from hidc.lexer import lex, SourceCode
from hidc.errors import LexerError

from pytest import raises

def lex_string(string):
    return [item.token for item in lex(SourceCode.from_string(string))]

def test_lex_int():
    assert lex_string('42') == [IntToken(42)]
    assert lex_string('1 2 3') == [IntToken(1), IntToken(2), IntToken(3)]
    assert lex_string('0xff') == [IntToken(0xff)]
    assert lex_string('0o70') == [IntToken(0o70)]
    assert lex_string('0b101') == [IntToken(0b101)]
    assert lex_string('1_000') == [IntToken(1000)]
    assert lex_string('0b1_1') == [IntToken(0b11)]
    assert lex_string('_1000') == [Ident('_1000')]

def test_weird_int():
    # At least for the time being, I'm not producing lexer errors for
    # stuff that looks a bit like ints but aren't.  Instead it will be
    # handled by by the parser.
    assert lex_string('0_') == [IntToken(0), Ident('_')]
    assert lex_string('1__000') == [IntToken(1), Ident('__000')]
    assert lex_string('0b') == [IntToken(0), Ident('b')]
    assert lex_string('0b_') == [IntToken(0), Ident('b_')]
    assert lex_string('0b_0') == [IntToken(0), Ident('b_0')]
    assert lex_string('0b0_') == [IntToken(0), Ident('_')]

def test_lex_string():
    assert lex_string(r'"hello"') == [StringToken(b'hello')]
    assert lex_string(r'""') == [StringToken(b'')]
    assert lex_string(r'"\""') == [StringToken(b'"')]
    assert lex_string(r'"\\"') == [StringToken(b'\\')]
    assert lex_string(r'"\n"') == [StringToken(b'\n')]
    assert lex_string(r'"\xff"') == [StringToken(b'\xff')]
    assert lex_string(r'"\u{ff}"') != lex_string(r'"\xff"')
    assert lex_string(r'"\u{ff}"') == [StringToken(chr(0xff).encode('utf-8'))]
    assert lex_string(r'"\u{1F4A9}"') == lex_string(r'"💩"') == [StringToken('💩'.encode('utf-8'))]

    with raises(LexerError): lex_string(r'"')
    with raises(LexerError): lex_string('"\\')
    with raises(LexerError): lex_string('"\n"')
    with raises(LexerError): lex_string(r'"\u"')
    with raises(LexerError): lex_string(r'"\u{ff"')
    with raises(LexerError): lex_string(r'"\u{d800}"')
    with raises(LexerError): lex_string(r'"\u{110000}"')
    with raises(LexerError): lex_string(r'"\x"')
    with raises(LexerError): lex_string(r'"\"')

    # HiD is strict about escape sequences
    with raises(LexerError): lex_string(r'"\?"')

def test_lex_char():
    assert lex_string(r"'a'") == [CharToken(ord('a'))]
    assert lex_string(r"'\\'") == [CharToken(ord('\\'))]
    assert lex_string(r"'\n'") == [CharToken(ord('\n'))]
    assert lex_string(r"'\xff'") == [CharToken(0xff)]
    assert lex_string(r"'\u{42}'") == [CharToken(0x42)]

    with raises(LexerError): lex_string(r"'")
    with raises(LexerError): lex_string(r"''")
    with raises(LexerError): lex_string(r"'ab'")
    with raises(LexerError): lex_string(r"'\u{ff}'")
    with raises(LexerError): lex_string("'\n'")
    with raises(LexerError): lex_string(r"'\'")

def test_lex_ident():
    assert lex_string('potato42') == [Ident('potato42')]
    assert lex_string('!is_defeat') == [Ident.defeat('is_defeat')]
    assert lex_string('@is_you') == [Ident.you('is_you')]

    with raises(LexerError): lex_string('!42')
    with raises(LexerError): lex_string('@42')

def test_lex_keyword():
    assert lex_string('for') == [BlockToken.FOR]
    assert lex_string('for ever') == [BlockToken.FOR, Ident('ever')]
    assert lex_string('forever') == [Ident('forever')]
    assert lex_string('@forever') == [Ident.you('forever')]
    assert lex_string('!forever') == [Ident.defeat('forever')]

    with raises(LexerError): lex_string('@for')
    with raises(LexerError): lex_string('!for')

def test_lex_symbol():
    assert lex_string('x+1') == [Ident('x'), Op.ADD, IntToken(1)]
    assert lex_string('+=') == [IncAssignToken.IADD]

def test_large():
    assert lex_string('''
        void @is_you() {
            if (true) {
                println("Hello world!");
            }
        }
    ''') == [
        Type.VOID, Ident.you('is_you'),
        BracToken.LPAREN, BracToken.RPAREN, BracToken.LCURLY,
            BlockToken.IF, BracToken.LPAREN, BoolToken.TRUE, BracToken.RPAREN, BracToken.LCURLY,
                Ident('println'), BracToken.LPAREN,
                    StringToken(b'Hello world!'),
                BracToken.RPAREN, SepToken.SEMICOLON,
            BracToken.RCURLY,
        BracToken.RCURLY
    ]

def test_tokens():
    assert lex_string('true')[0].data == True
    assert lex_string('0')[0].data == 0
    assert lex_string(r'"\x42"')[0].data == b'\x42'
    assert lex_string(r"'\x42'")[0].data == 0x42
    assert lex_string('+=')[0].operator is lex_string('+')[0]
    assert lex_string('(')[0].pair is lex_string(')')[0]
    assert lex_string('@is_you')[0].base_name == 'is_you'
    assert lex_string('@is_you')[0].name == '@is_you'

def test_hash_eq():
    test = '''
    "a" 97 'a' a @a !a + += break ; if ( true ) { } void
    '''
    assert set(lex_string(test)) == set(lex_string(test))
    assert len(set(lex_string(test))) == len(test.strip().split())

def test_syntax():
    assert lex_string('// Comment!') == lex_string('') == []
    with raises(LexerError): lex_string('#')

def test_enum_sanity():
    assert Op.ADD != '+'
    assert str(Op.ADD) == Op.ADD.value == '+'
    assert Op('+') is Op.ADD
    assert isinstance(Op.ADD, Token)
    assert isinstance(Op.ADD.value, str)
    assert not isinstance(Op.ADD.value, Token)

    assert Flavor.YOU != '@'
    assert str(Flavor.YOU) == Flavor.YOU.value == '@'
    assert Flavor('@') is Flavor.YOU
