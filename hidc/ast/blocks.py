from .abc import Statement
from .expressions import BoolValue, Expression, FuncCall
from .statements import ReturnStatement, BreakStatement, ContinueStatement
from .symbols import DataType, Ident, Flavor
from hidc.lexer import Span, Cursor
from hidc.errors import TypeCheckError

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


@dc.dataclass(frozen=True)
class CodeBlock(Block):
    stmts: Sequence[Statement]
    span: Span

    @classmethod
    def empty(cls, cursor):
        return cls((), Span(cursor, cursor))

    def evaluate(self, env):
        new_env = env.new_child()
        return CodeBlock(tuple([
            stmt.evaluate(new_env)
            for stmt in self.stmts
        ]), self.span)

    def exit_modes(self):
        mode = ExitMode.NONE
        found_continue = False
        for stmt in self.stmts:
            if ExitMode.NONE not in mode or found_continue:
                # TODO: Previously I produced an error for dead code,
                #  but now I'm ignoring it.
                #  Reason being is I think the error might confuse
                #  people when playing around with try/undo.
                #  I think the ideal thing to do would be to strip out
                #  dead code, but unfortunately due to how I've written
                #  things, that's quite awkward.  :(
                #  Possibly move this stuff to evaluate() and update an
                #  exit_modes attribute (initially NONE I guess) on the
                #  Block.
                #  Another alternative would be to produce a warning.
                # raise TypeCheckError('Unreachable statement', stmt.span)
                break

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

        return mode


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
            init or CodeBlock.empty(start),
            cls(start, body, cond or BoolValue(True, start),
                CodeBlock((cont,), cont.span) if cont
                else CodeBlock.empty(start))
        ), Span(start, body.span.end))

    def evaluate(self, env):
        return LoopBlock(
            self.start, self.body.evaluate(env),
            self.cond.evaluate(env).coerce(DataType.BOOL),
            self.cont.evaluate(env)
        )

    def exit_modes(self):
        mode = self.body.exit_modes()

        # Check for trivial infinite loop
        if mode.replace(ExitMode.LOOP, ExitMode.NONE) == ExitMode.NONE:
            if isinstance(self.cond, BoolValue) and self.cond.data:
                return ExitMode.LOOP

        # This will always include NONE, which is what we want since
        # even if there's a return, the loop might not get run at all.
        return mode.replace(ExitMode.BREAK, ExitMode.NONE)


@dc.dataclass(frozen=True)
class IfBlock(ControlBlock):
    cond: Expression
    else_block: Block

    def evaluate(self, env):
        return IfBlock(
            self.start, self.body.evaluate(env),
            self.cond.evaluate(env).coerce(DataType.BOOL),
            self.else_block.evaluate(env)
        )

    def exit_modes(self):
        return self.body.exit_modes() | self.else_block.exit_modes()


@dc.dataclass(frozen=True)
class UndoBlock(ControlBlock):
    def exit_modes(self):
        return self.body.exit_modes()


@dc.dataclass(frozen=True)
class PreemptBlock(ControlBlock):
    def exit_modes(self):
        # The preempt block may be skipped
        return self.body.exit_modes() | ExitMode.NONE


@dc.dataclass(frozen=True)
class TryBlock(ControlBlock):
    handler: UndoBlock

    def evaluate(self, env):
        return TryBlock(
            self.start, self.body.evaluate(env),
            self.handler.evaluate(env)
        )

    def exit_modes(self):
        return self.body.exit_modes().replace(
            ExitMode.DEFEAT, self.handler.exit_modes()
        )
