from . import abc
from .symbols import Variable

import dataclasses as dc

from hidc.lexer import Span, Cursor
from hidc.lexer.tokens import Op


@dc.dataclass(frozen=True)
class Declaration(abc.Statement):
    var: Variable
    init: abc.Expression
    start: Cursor

    @property
    def span(self):
        return self.init.span | self.start


@dc.dataclass(frozen=True)
class Assignment(abc.Statement):
    lookup: abc.Assignable
    expr: abc.Expression

    @property
    def span(self):
        return self.lookup.span | self.expr.span


@dc.dataclass(frozen=True)
class IncAssignment(Assignment):
    op: Op


@dc.dataclass(frozen=True)
class ReturnStatement(abc.Statement):
    span: Span
    value: abc.Expression = None


@dc.dataclass(frozen=True)
class BreakStatement(abc.Statement):
    span: Span


@dc.dataclass(frozen=True)
class ContinueStatement(abc.Statement):
    span: Span
