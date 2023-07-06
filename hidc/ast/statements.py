from . import abc
from .symbols import Variable, DataType

import dataclasses as dc

from hidc.lexer import Span, Cursor
from .operators import Binary
from .expressions import Expression, Assignable, Volatile
from hidc.errors import TypeCheckError


@dc.dataclass(frozen=True)
class Declaration(abc.Statement):
    var: Variable
    init: Expression
    start: Cursor

    @property
    def span(self):
        return self.init.span | self.start

    def evaluate(self, env):
        if prev_decl := env.vars.get(self.var.name):
            # Allow re-evaluation if necessary
            if prev_decl is self:
                return self

            # Fine to shadow globals, so long as we are not in the
            # global scope.
            if env.vars.is_global or prev_decl is not env.vars.globals.get(self.var.name):
                raise TypeCheckError(
                    f'Redeclaration of variable {self.var.name}',
                    (prev_decl.span, self.span)
                )

        init = self.init.evaluate(env).coerce(self.var.type)
        if isinstance(init, Volatile):
            assert self.var.type.const
            raise TypeCheckError(
                f'Cannot declare {self.var.name} as const with '
                'non-const reference intializer',
                init.span
            )

        new_decl = Declaration(self.var, init, self.start)
        env.vars[self.var.name] = new_decl
        return new_decl


@dc.dataclass(frozen=True)
class Assignment(abc.Statement):
    lookup: Assignable
    expr: Expression

    @property
    def span(self):
        return self.lookup.span | self.expr.span

    def evaluate(self, env):
        lookup = self.lookup.evaluate(env)
        if not isinstance(lookup, Assignable) or lookup.const:
            raise TypeCheckError('Cannot assign to const', lookup.span)
        expr = self.expr.evaluate(env).coerce(lookup.type)
        return Assignment(lookup, expr)



@dc.dataclass(frozen=True)
class IncAssignment(Assignment):
    bin_op: type[Binary]
    op_span: Span

    def evaluate(self, env):
        equiv_assign = self.type_equiv_assignment().evaluate(env)

        return IncAssignment(
            equiv_assign.lookup, self.expr.evaluate(env),
            self.bin_op, self.op_span
        )

    def type_equiv_assignment(self):
        # Gives an assignment which is "equivalent" to the incremental
        # assignment for typechecking purposes, ie x += 1 --> x = x + 1
        # It is also truly equivalent in the case of variable lookup
        # assignments, but NOT in the case of array lookups.
        return Assignment(
            self.lookup,
            self.bin_op(self.op_span, self.lookup, self.expr)
        )


@dc.dataclass(frozen=True)
class ReturnStatement(abc.Statement):
    span: Span
    value: Expression = None

    def evaluate(self, env):
        # env.return_type will be None in global scope
        # This should never happen as the parser doesn't allow return
        # statements in the global scope, but might as well be sure.
        if env.return_type is None:
            raise TypeCheckError('Unexpected return statement', self.span)

        if self.value is not None:
            if env.return_type == DataType.VOID:
                raise TypeCheckError(
                    'Unexpected value after return in void function',
                    self.value.span
                )

            return ReturnStatement(
                self.span,
                self.value.evaluate(env).coerce(env.return_type)
            )
        elif env.return_type != DataType.VOID:
            raise TypeCheckError('Missing return value', self.span)

        return self


@dc.dataclass(frozen=True)
class BreakStatement(abc.Statement):
    span: Span

    def evaluate(self, env):
        return self


@dc.dataclass(frozen=True)
class ContinueStatement(abc.Statement):
    span: Span

    def evaluate(self, env):
        return self
