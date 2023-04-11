from . import abc
from .expressions import BoolValue
from hidc.lexer import Span, Cursor

import dataclasses as dc
from collections.abc import Sequence


@dc.dataclass(frozen=True)
class CodeBlock(abc.Block):
    stmts: Sequence[abc.Statement]
    span: Span

    @classmethod
    def empty(cls, cursor):
        return cls((), Span(cursor, cursor))


@dc.dataclass(frozen=True)
class ControlBlock(abc.Block):
    start: Cursor
    body: abc.Block

    @property
    def span(self):
        return self.body.span | self.start


@dc.dataclass(frozen=True)
class LoopBlock(ControlBlock):
    cond: abc.Expression
    cont: CodeBlock

    @classmethod
    def while_loop(cls, start, body, cond):
        return cls(start, body, cond, CodeBlock.empty(start))

    @classmethod
    def for_loop(cls, start, body, init, cond, cont):
        return CodeBlock((
            init or CodeBlock.empty(start),
            cls(start, body, cond or BoolValue(True, start),
                cont and CodeBlock((cont,), cont.span))
        ), Span(start, body.span.end))


@dc.dataclass(frozen=True)
class IfBlock(ControlBlock):
    cond: abc.Expression
    else_block: abc.Block


@dc.dataclass(frozen=True)
class UndoBlock(ControlBlock):
    pass


@dc.dataclass(frozen=True)
class PreemptBlock(ControlBlock):
    pass


@dc.dataclass(frozen=True)
class TryBlock(ControlBlock):
    handler: UndoBlock
