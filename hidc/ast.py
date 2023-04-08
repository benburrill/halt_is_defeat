from .tokens import OpToken, IdentToken, TypeToken
from .utils.scanner import Span, Cursor
from .utils.propertyclasses import *

from collections.abc import Sequence
from dataclasses import dataclass, field


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


class Statement:
    span: Span


class Expression(Statement):
    pass


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
    values: Sequence[Expression]
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
    args: Sequence[Expression]
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


@dataclass(frozen=True)
class ReturnStatement(Statement):
    span: Span
    value: Expression = None

@dataclass(frozen=True)
class BreakStatement(Statement):
    span: Span

@dataclass(frozen=True)
class ContinueStatement(Statement):
    span: Span

class Block(Statement):
    span: Span


@dataclass(frozen=True)
class CodeBlock(Block, Sequence):
    stmts: Sequence[Statement]
    span: Span

    @classmethod
    def empty(cls, cursor):
        return cls((), Span(cursor, cursor))

    def __getitem__(self, item):
        return self.stmts[item]

    def __len__(self):
        return len(self.stmts)

    def __bool__(self):
        return True


@dataclass(frozen=True)
class ControlBlock(Block):
    start: Cursor
    body: Block

    @property
    def span(self):
        return Span(self.start, self.body.span.end)


@add_properties
@dataclass(frozen=True)
class LoopBlock(ControlBlock):
    cond: Expression
    _cont: Statement = internal_field(default=None)
    cont: Block = field(init=False)

    @property_field
    @property
    def cont(self):
        if self._cont is None:
            return CodeBlock.empty(self.body.span.end)
        return self._cont

    @classmethod
    def for_loop(cls, start, body, init, cond, cont):
        return CodeBlock((
            init or CodeBlock.empty(start),
            cls(start, body, cond or BoolLiteral(True, start),
                cont and CodeBlock((cont,), cont.span))
        ), Span(start, body.span.end))


@dataclass(frozen=True)
class IfBlock(ControlBlock):
    cond: Expression
    _else_block: Block = internal_field(default=None)
    else_block: Block = field(init=False)

    @property_field
    @property
    def else_block(self):
        if self._else_block is None:
            return CodeBlock.empty(self.body.span.end)
        return self._else_block


@dataclass(frozen=True)
class UndoBlock(ControlBlock):
    pass


@dataclass(frozen=True)
class PreemptBlock(ControlBlock):
    pass


@dataclass(frozen=True)
class TryBlock(ControlBlock):
    handler: UndoBlock
