from .tokens import OpToken, IdentToken, TypeToken
from .utils.scanner import Span, Cursor

from dataclasses import dataclass
import typing as ty


@dataclass(frozen=True)
class DataType:
    token: TypeToken


@dataclass(frozen=True)
class ArrayType(DataType):
    pass


@dataclass(frozen=True)
class Variable:
    const: bool
    type: DataType
    name: str


class Expression:
    span: Span


class Statement:
    span: Span


@dataclass(frozen=True)
class BinaryOp(Expression):
    op: OpToken
    op_span: Span
    left: Expression
    right: Expression

    @property
    def span(self):
        return Span(self.left.span.start, self.right.span.end)


@dataclass(frozen=True)
class UnaryOp(Expression):
    op: OpToken
    op_span: Span
    expr: Expression

    @property
    def span(self):
        return Span(self.op_span.start, self.expr.span.end)


@dataclass(frozen=True)
class IntLiteral(Expression):
    data: int
    span: Span


@dataclass(frozen=True)
class StringLiteral(Expression):
    data: bytes
    span: Span


@dataclass(frozen=True)
class BoolLiteral(Expression):
    data: bool
    span: Span


@dataclass(frozen=True)
class ArrayLiteral(Expression):
    values: ty.Sequence[Expression]
    span: Span


# Not really an expression, but let's pretend:
@dataclass(frozen=True)
class ArrayInitializer(Expression):
    type: ArrayType
    length: Expression

    @property
    def span(self):
        return self.length.span


class Assignable(Expression):
    pass


@dataclass(frozen=True)
class VariableLookup(Assignable):
    var: str
    span: Span


@dataclass(frozen=True)
class ArrayLookup(Assignable):
    source: Expression
    index: Expression
    end: Cursor

    @property
    def span(self):
        return Span(self.source.span.start, self.end)


@dataclass(frozen=True)
class LengthLookup(Expression):
    source: Expression
    end: Cursor

    @property
    def span(self):
        return Span(self.source.span.start, self.end)


class FuncCall(Expression):
    func: IdentToken
    args: ty.Sequence[Expression]
    span: Span


@dataclass(frozen=True)
class Declaration(Statement):
    var: Variable
    init: Expression
    start: Cursor

    @property
    def span(self):
        return Span(self.start, self.init.span.end)


@dataclass(frozen=True)
class Assignment(Statement):
    lookup: Assignable
    expr: Expression

    @property
    def span(self):
        return Span(self.lookup.span.start, self.expr.span.end)


@dataclass(frozen=True)
class IncAssignment(Assignment):
    op: OpToken
