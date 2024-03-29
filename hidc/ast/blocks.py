from .abc import Statement
from .expressions import BoolValue, Expression, FuncCall
from .statements import ReturnStatement, BreakStatement, ContinueStatement
from .symbols import DataType, Ident, Flavor
from hidc.lexer import Span, Cursor
from hidc.errors import TypeCheckError, InternalCompilerError
from hidc.utils.data_abc import Abstract

import dataclasses as dc
from collections.abc import Sequence
from abc import abstractmethod
import enum

class ExitMode(enum.Flag):
    NONE = enum.auto()
    BREAK = enum.auto()
    LOOP = enum.auto()
    DEFEAT = enum.auto()
    RETURN = enum.auto()

    # replace is a bit of a misleading name, but I can't think of a
    # better one.
    def replace(self, old, new):
        return (self & ~old) | new

class Block(Statement):
    @abstractmethod
    def exit_modes(self):
        pass

    preemptive: Abstract[bool]


@dc.dataclass(frozen=True)
class CodeBlock(Block):
    stmts: Sequence[Statement]
    span: Span
    # I can't be bothered to change the tests, so I'm setting a default
    # value of False for preemptive and making it compare=False.
    # However, evaluate is NOT responsible for setting preemptive (since
    # we want it to be affected even by unreachable code), so whenever a
    # CodeBlock is constructed for realsies, preemptive should always be
    # passed.
    preemptive: bool = dc.field(default=False, compare=False)
    _exit_mode: ExitMode = dc.field(default=None, compare=False)

    @classmethod
    def empty(cls, cursor):
        return cls((), Span(cursor, cursor), preemptive=False)

    def evaluate(self, env):
        new_env = env.new_child()
        new_stmts = []

        mode = ExitMode.NONE
        found_continue = False
        for stmt in self.stmts:
            if ExitMode.NONE not in mode or found_continue:
                if env.options.get('unreachable_error', False):
                    raise TypeCheckError('Unreachable statement', stmt.span)
                break

            stmt = stmt.evaluate(new_env)
            new_stmts.append(stmt)

            match stmt:
                case Block():
                    mode = mode.replace(ExitMode.NONE, stmt.exit_modes())
                case ReturnStatement():
                    mode = mode.replace(ExitMode.NONE, ExitMode.RETURN)
                case BreakStatement():
                    mode = mode.replace(ExitMode.NONE, ExitMode.BREAK)
                case FuncCall(Ident('is_defeat', Flavor.DEFEAT), ()):
                    mode = mode.replace(ExitMode.NONE, ExitMode.DEFEAT)
                case FuncCall(Ident('all_is_win', Flavor.NONE), ()):
                    mode = mode.replace(ExitMode.NONE, ExitMode.LOOP)
                case FuncCall(Ident('all_is_broken', Flavor.NONE), ()):
                    mode = mode.replace(ExitMode.NONE, ExitMode.LOOP)
                case FuncCall(Ident(_, Flavor.DEFEAT)):
                    # This is probably pointless to track, but why not?
                    # Could use it to make TryBlock more clever, but not
                    # very usefully so.
                    mode |= ExitMode.DEFEAT
                case ContinueStatement():
                    found_continue = True

        return CodeBlock(tuple(new_stmts), self.span, self.preemptive, mode)

    def exit_modes(self):
        if self._exit_mode is None:
            raise InternalCompilerError('Unevaluated block')
        return self._exit_mode


@dc.dataclass(frozen=True)
class ControlBlock(Block):
    start: Cursor
    body: Block

    @property
    def span(self):
        return self.body.span | self.start

    def evaluate(self, env):
        return type(self)(self.start, self.body.evaluate(env))


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
            *((init,) if init else ()),
            cls(start, body, cond or BoolValue(True, start),
                CodeBlock((cont,), cont.span, preemptive=False) if cont
                else CodeBlock.empty(start))
        ), Span(start, body.span.end), preemptive=False)

    def evaluate(self, env):
        return LoopBlock(
            self.start, self.body.evaluate(env),
            self.cond.evaluate(env).cast(DataType.BOOL),
            self.cont.evaluate(env)
        )

    def exit_modes(self):
        mode = self.body.exit_modes()

        # Check for trivial infinite loop
        if ExitMode.BREAK not in mode:
            if isinstance(self.cond, BoolValue) and self.cond.data:
                return mode.replace(ExitMode.NONE, ExitMode.LOOP)

        # This will always include NONE, which is what we want since
        # even if there's a return, the loop might not get run at all.
        return mode.replace(ExitMode.BREAK, ExitMode.NONE)

    @property
    def preemptive(self):
        return self.body.preemptive


@dc.dataclass(frozen=True)
class IfBlock(ControlBlock):
    cond: Expression
    else_block: Block

    def evaluate(self, env):
        return IfBlock(
            self.start, self.body.evaluate(env),
            self.cond.evaluate(env).cast(DataType.BOOL),
            self.else_block.evaluate(env)
        )

    def exit_modes(self):
        return self.body.exit_modes() | self.else_block.exit_modes()

    @property
    def preemptive(self):
        return self.body.preemptive or self.else_block.preemptive


@dc.dataclass(frozen=True)
class UndoBlock(ControlBlock):
    def exit_modes(self):
        return self.body.exit_modes()

    @property
    def preemptive(self):
        return self.body.preemptive


@dc.dataclass(frozen=True)
class StopBlock(ControlBlock):
    def exit_modes(self):
        return self.body.exit_modes()

    @property
    def preemptive(self):
        return self.body.preemptive


@dc.dataclass(frozen=True)
class PreemptBlock(ControlBlock):
    preemptive = True
    def exit_modes(self):
        # The preempt block may be skipped
        return self.body.exit_modes() | ExitMode.NONE


@dc.dataclass(frozen=True)
class TryBlock(ControlBlock):
    handler: UndoBlock | StopBlock
    preemptive = False

    def evaluate(self, env):
        return TryBlock(
            self.start, self.body.evaluate(env),
            self.handler.evaluate(env)
        )

    def exit_modes(self):
        return self.body.exit_modes().replace(
            ExitMode.DEFEAT, self.handler.exit_modes()
        )
