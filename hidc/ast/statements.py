from . import abc
from .symbols import Variable

import dataclasses as dc

from hidc.lexer import Span, Cursor
from .operators import Binary
from .expressions import Expression, Assignable


@dc.dataclass(frozen=True)
class Declaration(abc.Statement):
    var: Variable
    init: Expression
    start: Cursor

    @property
    def span(self):
        return self.init.span | self.start


@dc.dataclass(frozen=True)
class Assignment(abc.Statement):
    lookup: Assignable
    expr: Expression

    @property
    def span(self):
        return self.lookup.span | self.expr.span


@dc.dataclass(frozen=True)
class IncAssignment(Assignment):
    bin_op: type[Binary]


@dc.dataclass(frozen=True)
class ReturnStatement(abc.Statement):
    span: Span
    value: Expression = None


@dc.dataclass(frozen=True)
class BreakStatement(abc.Statement):
    span: Span


@dc.dataclass(frozen=True)
class ContinueStatement(abc.Statement):
    span: Span
