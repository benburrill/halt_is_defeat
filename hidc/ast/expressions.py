from . import abc
from .symbols import *
from hidc.lexer import Span, Cursor
from hidc.errors import TypeCheckError

import dataclasses as dc
from collections.abc import Sequence

from hidc.utils.data_abc import Abstract
from abc import abstractmethod


class Expression(abc.Statement):
    type: Abstract[Type]

    def cast(self, new_type):
        if self.type == new_type:
            return self

        match (self.type, new_type):
            case IntToByte.map:
                return IntToByte(self)
            case ByteToInt.map:
                return ByteToInt(self)
            case (DataType.STRING | ArrayType(), DataType.BOOL):
                return LengthLookup(self, self.span.end).cast(new_type)
            case (_, DataType.BOOL):
                return IntToBool(self.cast(DataType.INT))
            case (DataType.BOOL, _):
                return BoolToByte(self).cast(new_type)
            case StringToByteArray.map:
                return StringToByteArray(self)
            case (ArrayType(T1, const=False), ArrayType(T2, const=True)) if T1 == T2:
                return Volatile(self)

        raise TypeCheckError(f'{self.type} is not {new_type}', self.span)

    def coercible(self, new_type):
        if self.type == new_type:
            return True

        if isinstance(self.type, ArrayType):
            return ArrayType(self.type.el_type, const=True) == new_type

        return (self.type, new_type) in {
            ByteToInt.map,
            StringToByteArray.map
        }

    def coerce(self, new_type):
        if self.coercible(new_type):
            return self.cast(new_type)

        raise TypeCheckError(f'{self.type} is not {new_type}', self.span)


@dc.dataclass(frozen=True)
class TypeCast(Expression):
    expr: Expression

    @property
    @abstractmethod
    def map(self):
        pass

    @property
    def type(self):
        expr_type, new_type = self.map
        assert expr_type == self.expr.type
        return new_type

    def evaluate(self, env):
        return self.expr.evaluate(env).cast(self.type)

    @property
    def span(self):
        return self.expr.span


class ByteToInt(TypeCast):
    map = (DataType.BYTE, DataType.INT)

class IntToByte(TypeCast):
    map = (DataType.INT, DataType.BYTE)

class IntToBool(TypeCast):
    map = (DataType.INT, DataType.BOOL)

class BoolToByte(TypeCast):
    # in code generation this typecast does nothing
    map = (DataType.BOOL, DataType.BYTE)

class StringToByteArray(TypeCast):
    map = (DataType.STRING, ArrayType(DataType.BYTE, const=True))


# Volatile arrays are non-const arrays pretending to be const
# Should only be accepted in function calls, not declarations.
@dc.dataclass(frozen=True)
class Volatile(Expression):
    expr: Expression

    @property
    def type(self):
        assert not self.expr.type.const
        return ArrayType(self.expr.type.el_type, const=True)

    def evaluate(self, env):
        return self.expr.evaluate(env).coerce(self.type)

    # Volatile arrays can be unwrapped back to non-const arrays
    # Idea is that explicit type-casts to arrays, eg x is byte[] will be
    # const, so produce Volatile if x is non-const, but be coercible
    # back to non-const if you do something like byte[] y = x is byte[]
    # x is byte[] is pretty much only useful if x is a string, and hence
    # const, but it's still important for the sake of consistency.
    def coercible(self, new_type):
        return self.expr.coercible(new_type)

    def cast(self, new_type):
        return self.expr.cast(new_type)

    @property
    def span(self):
        return self.expr.span


class Assignable(Expression):
    const: Abstract[bool]


@dc.dataclass(frozen=True)
class VariableLookup(Assignable):
    var: Variable | UnresolvedName
    span: Span

    @property
    def type(self):
        return self.var.type

    @property
    def const(self):
        return self.var.const

    def evaluate(self, env):
        if isinstance(self.var, UnresolvedName):
            try:
                decl = env.vars[self.var.name]
            except KeyError:
                raise TypeCheckError(f'{self.var.name} is empty', self.span) from None

            var = decl.var
            # Only declarations are allowed in global scope, so we can
            # treat variables as const even if they're not.
            if var.const or env.vars.is_global:
                if isinstance(decl.init, PrimitiveValue):
                    return decl.init.at(self.span)
            return VariableLookup(var, self.span)
        return self


@dc.dataclass(frozen=True)
class ArrayLookup(Assignable):
    source: Expression
    index: Expression
    end: Cursor

    @property
    def span(self):
        return self.source.span | self.end

    @property
    def type(self):
        if self.source.type == DataType.STRING:
            return DataType.BYTE
        return self.source.type.el_type

    @property
    def const(self):
        if self.source.type == DataType.STRING:
            return False
        return self.source.type.const

    def evaluate(self, env):
        # TODO: get primitive value if possible
        #  must be careful though, see LengthLookup
        source = self.source.evaluate(env)
        if not isinstance(source.type, ArrayType) and source.type != DataType.STRING:
            raise TypeCheckError('Must be array or string', self.source.span)

        if isinstance(source, ArrayLiteral):
            # TODO: maybe make this a method of ArrayLiteral, but I'm
            #  pretty sure this is the only place where this is needed.
            if source.type.el_type == DataType.EMPTY:
                raise TypeCheckError('Array type is ambiguous', source.span)
            source = source.coerce(source.type)  # coerce elements
        elif isinstance(source.type, ArrayType):
            assert source.type.el_type != DataType.EMPTY

        return ArrayLookup(
            source, self.index.evaluate(env).coerce(DataType.INT),
            self.end
        )


@dc.dataclass(frozen=True)
class LengthLookup(Expression):
    type = DataType.INT
    source: Expression
    end: Cursor

    @property
    def span(self):
        return self.source.span | self.end

    def evaluate(self, env):
        # TODO: get primitive value if possible
        #  If variable, look at what it's assigned to, but it must be
        #  const-ish since you can reassign strings.
        #  For ArrayInitializers we have to be careful, can only use its
        #  length if its an IntValue.
        #  For ArrayLiterals we can always use the length.
        #  -- Actually for casting reasons, we want to get the primitive
        #  value in cast as that's what Expression.cast expects.
        #  -- but that's a bit of a pain because variables :(
        #  -- may need to redesign
        #  ----- expressions in array literals do need to be evaluated
        #  though, so it's only really valid to get the length if it's
        #  an array literal assigned to a variable.
        source = self.source.evaluate(env)
        if not isinstance(source.type, ArrayType) and source.type != DataType.STRING:
            raise TypeCheckError('Must be array or string', self.source.span)
        return LengthLookup(source, self.end)


@dc.dataclass(frozen=True)
class FuncCall(Expression):
    func: Ident
    args: Sequence[Expression]
    span: Span
    type: Type = DataType.EMPTY

    def evaluate(self, env):
        args = tuple(arg.evaluate(env) for arg in self.args)
        arg_sig = tuple(arg.type for arg in args)
        sig = arg_sig

        try:
            func = env.funcs[self.func][arg_sig]
        except KeyError:
            for sig, func in env.funcs.get(self.func, {}).items():
                if len(sig) == len(args):
                    for arg, sig_type in zip(args, sig):
                        if not arg.coercible(sig_type):
                            break
                    else:
                        break
            else:
                params = ', '.join(map(str, arg_sig))

                help_msg = ''
                if self.func.name in {'print', 'println'}:
                    new_name = self.func.name.replace('print', 'write')
                    # Only testing for exact type match here, but should
                    # catch most misspellings.
                    if arg_sig in env.funcs[Ident(new_name)]:
                        help_msg = f' -- Did you mean {new_name}({params})?'

                raise TypeCheckError(
                    f'No matching function for signature {self.func}({params}){help_msg}',
                    self.span
                ) from None

        return FuncCall(
            self.func, tuple([
                arg.coerce(sig_type)
                for arg, sig_type in zip(args, sig)
            ]),
            self.span, func.ret_type
        )


# Parameters are also not really expressions, but we're going to pretend
# they are the RHS of imaginary declarations.
@dc.dataclass(frozen=True)
class Parameter(Expression):
    var: Variable
    span: Span

    def evaluate(self, env):
        return self

    @property
    def type(self):
        return self.var.type


@dc.dataclass(frozen=True)
class PrimitiveValue(Expression):
    data: object
    span: Span

    def evaluate(self, env):
        return self

    def at(self, span):
        return type(self)(self.data, span)


@dc.dataclass(frozen=True)
class IntValue(PrimitiveValue):
    type = DataType.INT
    data: int
    shrinkable: bool = True
    is_char: bool = dc.field(default=False, compare=False)

    def cast(self, new_type, *, implicit=False):
        if new_type == DataType.BOOL:
            return BoolValue(bool(self.data), self.span)
        elif new_type == DataType.BYTE:
            # TODO: we should probably do self.data & 0xFF
            #  Should have result that 4 / (258 is byte) produces 2, as
            #  it would if evaluated at runtime.
            #  For that matter I should also track word size in env and
            #  use for (signed) wraparound in arithmetic evaluation.
            #  Also when we create IntValues to begin with...  and maybe
            #  more places I forget.
            return ByteValue(self.data, self.span, self.shrinkable, self.is_char)
        elif new_type == DataType.INT:
            # Whenever a ByteValue is implicitly coerced to an IntValue,
            # it should be shrinkable back to byte.
            return IntValue(self.data, self.span, implicit, self.is_char)
        return super().cast(new_type)

    def coercible(self, new_type):
        return (
            super().coercible(new_type) or
            self.shrinkable and new_type == DataType.BYTE
        )

    def coerce(self, new_type):
        # (1 + 1) should be coercible to byte, but should (2 is int) be?
        # I think not.  So I'm awkwardly adding this "implicit" argument
        # to retain literal status for implicit coersions only.
        # Regardless, (2 is byte) is coercible to int.
        # TODO: different set of overrideable methods / other mechanism?
        #  Whatever I do, I want all possible casts defined together.
        if self.coercible(new_type):
            return self.cast(new_type, implicit=True)
        return super().coerce(new_type)

    # Substitution makes IntValues no longer be shrinkable and instead
    # act like normal ints in type coercion.
    def at(self, span):
        return type(self)(self.data, span, shrinkable=False, is_char=self.is_char)


class ByteValue(IntValue):
    type = DataType.BYTE


@dc.dataclass(frozen=True)
class BoolValue(PrimitiveValue):
    type = DataType.BOOL
    data: bool

    def cast(self, new_type):
        if new_type == DataType.INT:
            return IntValue(int(self.data), self.span)
        elif new_type == DataType.BYTE:
            return ByteValue(int(self.data), self.span)
        return super().cast(new_type)


@dc.dataclass(frozen=True)
class StringValue(PrimitiveValue):
    type = DataType.STRING
    data: bytes

    def cast(self, new_type):
        if new_type == DataType.BOOL:
            return BoolValue(bool(self.data), self.span)
        return super().cast(new_type)


@dc.dataclass(frozen=True)
class ArrayLiteral(Expression):
    values: Sequence[Expression]
    span: Span
    type: Type = ArrayType(DataType.EMPTY, const=True)
    type_locked: bool = False

    def coercible(self, new_type):
        if isinstance(new_type, ArrayType):
            # type_locked disables type coercion between element types,
            # but still allows coercion of constness.
            if self.type_locked:
                return self.type.el_type == new_type.el_type
            return all(v.coercible(new_type.el_type) for v in self.values)
        return False

    def cast(self, new_type):
        if isinstance(new_type, ArrayType):
            # After a cast is performed, type coercion of array literals
            # to other types is disabled, even if the elements are still
            # coercible.
            return ArrayLiteral(tuple([
                v.cast(new_type.el_type) for v in self.values
            ]), self.span, new_type, type_locked=True)
        return super().cast(new_type)

    def evaluate(self, env):
        if not self.values:
            return self

        values = tuple([v.evaluate(env) for v in self.values])

        # Last value for each unique type in order of occurrence
        # (first value would be better, but only affects error message)
        for value in {v.type: v for v in values}.values():
            if isinstance(value.type, ArrayType):
                raise TypeCheckError('Nested arrays are unsupported', value.span)

            # The preferred element type for an array literal, ie the
            # type used preferentially to match function signatures, is
            # the first type that all other elements may be coerced to.
            # However, array literals may be coerced to any array type
            # so long as the elements are coercible to that type.
            #
            # We are intentionally NOT actually coercing the values to
            # that type here, that should be done when used.  The reason
            # is that [1, b] is preferentially int[] but is coercible to
            # byte[].  But if we coerce b to int, we can't go back.
            if all(v.coercible(value.type) for v in values):
                return ArrayLiteral(values, self.span, ArrayType(value.type, const=True))
        raise TypeCheckError('Array type is unresolvable', self.span)

    # @property
    # def length(self):
    #     return IntValue(len(self.values), self.span)


# Not really an expression, but let's pretend:
@dc.dataclass(frozen=True)
class ArrayInitializer(Expression):
    type: ArrayType
    length: Expression

    @property
    def span(self):
        return self.length.span

    def evaluate(self, env):
        return ArrayInitializer(
            self.type, self.length.evaluate(env).coerce(DataType.INT)
        )
