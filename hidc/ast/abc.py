from hidc.lexer import Span
from abc import ABC

class Statement(ABC):
    span: Span


class Block(Statement):
    pass


class Expression(Statement):
    pass


class Assignable(Expression):
    pass
