from hidc.tokens import *
from hidc.lexer import lex
from hidc.scanner import SourceCode
from hidc.errors import LexerError

from pytest import raises

def lex_string(string):
    return [span.item for span in lex(SourceCode.from_string(string))]

def test_lex_int():
    assert lex_string('42') == [IntToken(42)]
    assert lex_string('1 2 3') == [IntToken(1), IntToken(2), IntToken(3)]
    assert lex_string('0xff') == [IntToken(0xff)]
    assert lex_string('0o70') == [IntToken(0o70)]
    assert lex_string('0b101') == [IntToken(0b101)]
    assert lex_string('1_000') == [IntToken(1000)]
    assert lex_string('0b1_1') == [IntToken(0b11)]
    assert lex_string('_1000') == [IdentToken('_1000', Flavor.NONE)]

def test_weird_int():
    # At least for the time being, I'm not producing lexer errors for
    # stuff that looks a bit like ints but aren't.  Instead it will be
    # handled by by the parser.
    assert lex_string('0_') == [IntToken(0), IdentToken('_', Flavor.NONE)]
    assert lex_string('0b') == [IntToken(0), IdentToken('b', Flavor.NONE)]
    assert lex_string('0b_') == [IntToken(0), IdentToken('b_', Flavor.NONE)]
    assert lex_string('0b_0') == [IntToken(0), IdentToken('b_0', Flavor.NONE)]
    assert lex_string('0b0_') == [IntToken(0), IdentToken('_', Flavor.NONE)]

def test_lex_string():
    assert lex_string(r'"hello"') == [StringToken(b'hello')]
    assert lex_string(r'""') == [StringToken(b'')]
    assert lex_string(r'"\""') == [StringToken(b'"')]
    assert lex_string(r'"\\"') == [StringToken(b'\\')]
    assert lex_string(r'"\n"') == [StringToken(b'\n')]
    assert lex_string(r'"\xff"') == [StringToken(b'\xff')]
    assert lex_string(r'"\u{ff}"') != lex_string(r'"\xff"')
    assert lex_string(r'"\u{ff}"') == [StringToken('\xff'.encode())]

    with raises(LexerError): lex_string(r'"')
    with raises(LexerError): lex_string('"\\')
    with raises(LexerError): lex_string('"\n"')
    with raises(LexerError): lex_string(r'"\u"')
    with raises(LexerError): lex_string(r'"\u{ff"')
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
    assert lex_string('potato42') == [IdentToken('potato42', Flavor.NONE)]
    assert lex_string('!is_defeat') == [IdentToken('is_defeat', Flavor.DEFEAT)]
    assert lex_string('@is_you') == [IdentToken('is_you', Flavor.YOU)]

    with raises(LexerError): lex_string('!42')
    with raises(LexerError): lex_string('@42')

def test_lex_keyword():
    assert lex_string('for') == [BlockToken.FOR]
    assert lex_string('for ever') == [BlockToken.FOR, IdentToken('ever', Flavor.NONE)]
    assert lex_string('forever') == [IdentToken('forever', Flavor.NONE)]
    assert lex_string('@forever') == [IdentToken('forever', Flavor.YOU)]
    assert lex_string('!forever') == [IdentToken('forever', Flavor.DEFEAT)]

    with raises(LexerError): lex_string('@for')
    with raises(LexerError): lex_string('!for')

def test_lex_symbol():
    assert lex_string('x+1') == [IdentToken('x', Flavor.NONE), OpToken.ADD, IntToken(1)]

def test_large():
    assert lex_string('''
        void @is_you() {
            if (true) {
                println("Hello world!");
            }
        }
    ''') == [
        TypeToken.VOID, IdentToken('is_you', Flavor.YOU),
        BracToken.LPAREN, BracToken.RPAREN, BracToken.LCURLY,
            BlockToken.IF, BracToken.LPAREN, BoolToken.TRUE, BracToken.RPAREN, BracToken.LCURLY,
                IdentToken('println', Flavor.NONE), BracToken.LPAREN,
                    StringToken(b'Hello world!'),
                BracToken.RPAREN, SepToken.SEMICOLON,
            BracToken.RCURLY,
        BracToken.RCURLY
    ]

def test_hash_eq():
    test = '''
    "a" 97 'a' a @a !a + += break; if (true) {} void
    '''
    assert set(lex_string(test)) == set(lex_string(test))
    assert len(set(lex_string(test))) == 17

def test_invalid_syntax():
    with raises(LexerError): lex_string('`')

def test_enum_sanity():
    assert OpToken.ADD != '+'
    assert str(OpToken.ADD) == OpToken.ADD.value == '+'
    assert OpToken('+') is OpToken.ADD
    assert isinstance(OpToken.ADD, Token)
    assert isinstance(OpToken.ADD.value, str)
    assert not isinstance(OpToken.ADD.value, Token)

    assert Flavor.YOU != '@'
    assert str(Flavor.YOU) == Flavor.YOU.value == '@'
    assert Flavor('@') is Flavor.YOU
