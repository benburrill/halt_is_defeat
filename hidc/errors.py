def _message(m):
    @classmethod
    def builder(cls, *args, **format_vars):
        return cls(m.format(**format_vars), *args)
    return builder

class CompilerError(Exception):
    def __init__(self, message, context):
        from hidc.lexer import Span, Cursor
        super().__init__(message)
        if isinstance(context, Span) or isinstance(context, Cursor):
            self.context = (context,)
        else:
            self.context = tuple(context)

    def get_info(self, source):
        message = source.filename
        if self.context:
            message += f':{self.context[-1].start}'
        message += f': {self}'

        for span in self.context:
            # Mimic gcc error messages
            line = span.start.line
            message += f'\n{line + 1:5} | {source.lines[line]}'

        # last context entry is the focus -- show arrow there
        # actually looks like gcc does it the other way around and draws
        # arrows for all lines, but eh...
        if self.context:
            col = self.context[-1].start.col
            message += f'\n      | ' + (' ' * col) + '^'

        return message


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

class CodeGenError(CompilerError):
    pass

# TODO: when or if should this replace assertions?
class InternalCompilerError(Exception):
    pass
