from . import tokens
from . import readers
from .scanner import Scanner, Span, Cursor, SourceCode
from hidc.errors import LexerError

import dataclasses as dc

@dc.dataclass(frozen=True)
class Lexeme:
    token: tokens.Token
    span: Span


def lex(source):
    tok_readers = [
        readers.read_symbol_token, readers.read_ident_or_keyword_token,
        readers.read_int_token, readers.read_string_token, readers.read_char_token
    ]

    scan = Scanner(source)
    marker = scan.mark()

    while True:
        readers.skip_whitespace(scan)

        if not scan:
            return marker.cursor

        marker = scan.mark()
        for reader in tok_readers:
            tok = reader(scan)
            if tok is not None:
                yield Lexeme(tok, marker.advance())
                break
        else:
            raise LexerError.unhelpful(scan.cursor)


