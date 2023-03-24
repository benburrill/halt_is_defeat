def _message(m):
    @classmethod
    def builder(cls, *args, **format_vars):
        return cls(m.format(**format_vars), *args)
    return builder

class CompilerError(Exception):
    pass

class LexerError(CompilerError):
    def __init__(self, message, marker):
        super().__init__(message)
        self.marker = marker

    unhelpful = _message('Invalid syntax')
    expected = _message('Invalid syntax, expected {need}')
    end_of_line = _message('Unexpected end of line')
    eol_string = _message('Unterminated string literal')
    bad_unicode = _message('Invalid unicode codepoint: {cp:X}')
    bad_escape = _message('Invalid escape sequence: \\{seq}')
    bad_hex_escape = _message('Truncated \\{seq} escape sequence')
    too_many_bytes = _message('Unicode characters in char literals must'
                              ' be encodable as a single byte in UTF-8')
    bad_ident = _message('Invalid identifier starting with {prefix!r}')
