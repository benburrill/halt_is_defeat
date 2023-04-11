from . import abc
from .symbols import *
from hidc.lexer import Span, Cursor
from hidc.lexer.tokens import Op

import dataclasses as dc
from collections.abc import Sequence


@dc.dataclass(frozen=True)
class BinaryOp(abc.Expression):
    op: Op
    op_span: Span
    left: abc.Expression
    right: abc.Expression

    @property
    def span(self):
        return self.left.span | self.right.span


@dc.dataclass(frozen=True)
class UnaryOp(abc.Expression):
    op: Op
    op_span: Span
    expr: abc.Expression

    @property
    def span(self):
        return self.op_span | self.expr.span


@dc.dataclass(frozen=True)
class IntValue(abc.PrimitiveValue):
    data: int
    span: Span


class ByteValue(IntValue):
    pass


@dc.dataclass(frozen=True)
class StringValue(abc.PrimitiveValue):
    data: bytes
    span: Span


@dc.dataclass(frozen=True)
class BoolValue(abc.PrimitiveValue):
    data: bool
    span: Span


@dc.dataclass(frozen=True)
class ArrayLiteral(abc.Expression):
    values: Sequence[abc.Expression]
    span: Span

    @property
    def length(self):
        return IntValue(len(self.values), self.span)

    @property
    def concrete(self):
        return all(val.concrete for val in self.values)


# Not really an expression, but let's pretend:
@dc.dataclass(frozen=True)
class ArrayInitializer(abc.Expression):
    type: ArrayType
    length: abc.Expression

    @property
    def span(self):
        return self.length.span


@dc.dataclass(frozen=True)
class VariableLookup(abc.Assignable):
    var: str
    span: Span


@dc.dataclass(frozen=True)
class ArrayLookup(abc.Assignable):
    source: abc.Expression
    index: abc.Expression
    end: Cursor

    @property
    def span(self):
        return self.source.span | self.end


@dc.dataclass(frozen=True)
class LengthLookup(abc.Expression):
    source: abc.Expression
    end: Cursor

    @property
    def span(self):
        return self.source.span | self.end


@dc.dataclass(frozen=True)
class FuncCall(abc.Expression):
    func: Ident
    args: Sequence[abc.Expression]
    span: Span
