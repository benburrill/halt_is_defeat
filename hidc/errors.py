def _message(m):
    @classmethod
    def builder(cls, *args, **format_vars):
        return cls(m.format(**format_vars), *args)
    return builder

class CompilerError(Exception):
    def __init__(self, message, span):
        super().__init__(message)
        self.span = span

    def get_info(self, source):
        # Mimic gcc error messages
        line = self.span.start.line
        col = self.span.start.col
        return (f'{source.filename}:{self.span.start}: {self}\n'
                f'{line + 1:5} | {source.lines[line]}\n'
                f'      | ' + (' ' * col) + '^')

class LexerError(CompilerError):
    unhelpful = _message('Invalid syntax')
    expected = _message('Invalid syntax, expected {need}')

class ParserError(CompilerError):
    @classmethod
    async def expected(cls, need):
        from hidc.parser.rules import CurrentNode
        if node := await CurrentNode():
            return cls(
                f'Expected {need}, got unexpected token: {node.head.token}',
                node.head.span
            )

        return cls(f'Expected {need}, found end of file', node.value)

class TypeCheckError(CompilerError):
    pass
