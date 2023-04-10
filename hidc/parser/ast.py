from hidc.lexer.tokens import Op, Ident, Type
from hidc.lexer.scanner import Span, Cursor

from collections.abc import Sequence
import dataclasses as dc


@dc.dataclass(frozen=True)
class DataType:
    token: Type


@dc.dataclass(frozen=True)
class ArrayType(DataType):
    pass


@dc.dataclass(frozen=True)
class Variable:
    const: bool
    type: DataType
    name: str


class Statement:
    span: Span


class Expression(Statement):
    pass


@dc.dataclass(frozen=True)
class BinaryOp(Expression):
    op: Op
    op_span: Span
    left: Expression
    right: Expression

    @property
    def span(self):
        return self.left.span | self.right.span


@dc.dataclass(frozen=True)
class UnaryOp(Expression):
    op: Op
    op_span: Span
    expr: Expression

    @property
    def span(self):
        return self.op_span | self.expr.span


@dc.dataclass(frozen=True)
class IntLiteral(Expression):
    data: int
    span: Span


@dc.dataclass(frozen=True)
class StringLiteral(Expression):
    data: bytes
    span: Span


@dc.dataclass(frozen=True)
class BoolLiteral(Expression):
    data: bool
    span: Span


@dc.dataclass(frozen=True)
class ArrayLiteral(Expression):
    values: Sequence[Expression]
    span: Span

    @property
    def length(self):
        return IntLiteral(len(self.values), self.span)


# Not really an expression, but let's pretend:
@dc.dataclass(frozen=True)
class ArrayInitializer(Expression):
    type: ArrayType
    length: Expression

    @property
    def span(self):
        return self.length.span


class Assignable(Expression):
    pass


@dc.dataclass(frozen=True)
class VariableLookup(Assignable):
    var: str
    span: Span


@dc.dataclass(frozen=True)
class ArrayLookup(Assignable):
    source: Expression
    index: Expression
    end: Cursor

    @property
    def span(self):
        return self.source.span | self.end


@dc.dataclass(frozen=True)
class LengthLookup(Expression):
    source: Expression
    end: Cursor

    @property
    def span(self):
        return self.source.span | self.end


@dc.dataclass(frozen=True)
class FuncCall(Expression):
    func: Ident
    args: Sequence[Expression]
    span: Span


@dc.dataclass(frozen=True)
class Declaration(Statement):
    var: Variable
    init: Expression
    start: Cursor

    @property
    def span(self):
        return self.init.span | self.start


@dc.dataclass(frozen=True)
class Assignment(Statement):
    lookup: Assignable
    expr: Expression

    @property
    def span(self):
        return self.lookup.span | self.expr.span


@dc.dataclass(frozen=True)
class IncAssignment(Assignment):
    op: Op


@dc.dataclass(frozen=True)
class ReturnStatement(Statement):
    span: Span
    value: Expression = None

@dc.dataclass(frozen=True)
class BreakStatement(Statement):
    span: Span

@dc.dataclass(frozen=True)
class ContinueStatement(Statement):
    span: Span

class Block(Statement):
    span: Span


@dc.dataclass(frozen=True)
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


@dc.dataclass(frozen=True)
class ControlBlock(Block):
    start: Cursor
    body: Block

    @property
    def span(self):
        return self.body.span | self.start


@dc.dataclass(frozen=True)
class LoopBlock(ControlBlock):
    cond: Expression
    cont: CodeBlock

    @classmethod
    def while_loop(cls, start, body, cond):
        return cls(start, body, cond, CodeBlock.empty(start))

    @classmethod
    def for_loop(cls, start, body, init, cond, cont):
        return CodeBlock((
            init or CodeBlock.empty(start),
            cls(start, body, cond or BoolLiteral(True, start),
                cont and CodeBlock((cont,), cont.span))
        ), Span(start, body.span.end))


@dc.dataclass(frozen=True)
class IfBlock(ControlBlock):
    cond: Expression
    else_block: Block


@dc.dataclass(frozen=True)
class UndoBlock(ControlBlock):
    pass


@dc.dataclass(frozen=True)
class PreemptBlock(ControlBlock):
    pass


@dc.dataclass(frozen=True)
class TryBlock(ControlBlock):
    handler: UndoBlock


@dc.dataclass(frozen=True)
class FuncSignature:
    name: Ident
    params: Sequence[Variable]


@dc.dataclass(frozen=True)
class FuncDeclaration(Statement):
    span: Span
    ret_type: DataType
    signature: FuncSignature
    body: CodeBlock


@dc.dataclass(frozen=True)
class Program:
    var_decls: Sequence[Declaration]
    func_decls: Sequence[FuncDeclaration]
