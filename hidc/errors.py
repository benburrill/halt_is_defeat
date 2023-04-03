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