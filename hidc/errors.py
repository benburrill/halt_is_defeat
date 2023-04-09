def _message(m):
    @classmethod
    def builder(cls, *args, **format_vars):
        return cls(m.format(**format_vars), *args)
    return builder

class CompilerError(Exception):
    def __init__(self, message, span):
        super().__init__(message)
        self.span = span

class LexerError(CompilerError):
    unhelpful = _message('Invalid syntax')
    expected = _message('Invalid syntax, expected {need}')

class ParserError(CompilerError):
    @classmethod
    async def expected(cls, need):
        from hidc.parser import CurrentNode
        if node := await CurrentNode():
            return cls(
                f'Expected {need}, got unexpected token: {node.head.token}',
                node.head.span
            )

        return cls(f'Expected {need}, found end of file', node.value)
