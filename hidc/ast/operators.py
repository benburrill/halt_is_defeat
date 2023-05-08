from .symbols import DataType
from hidc.lexer import Span
from hidc.errors import TypeCheckError

import dataclasses as dc
from abc import abstractmethod
from .expressions import Expression, BoolValue, IntValue, PrimitiveValue
import operator

# kinds of operator:
# - unary arithmetic: (~int) -> int
# - binary arithmetic: (~int, ~int) -> int
# - binary comparison: (~int, ~int) -> bool
# - equality operators: (string?|~int|bool, string?|~int|bool) -> bool
#      Fast equality checks on string wouldn't be possible if we have
#      command line arguments.  Other than that though, strings could be
#      made totally unique -- but we could at least check if both
#      strings are in the string table and if so do a fast check.
#      However just allowing ~int and bool makes things easier as we can
#      simply cast arguments to INT
# - unary logical: (any) -> bool
# - binary logical: (any, any) -> bool
# - is: (expr, type) -> expr.cast(type)
# - speculation: (any, any) -> any

# arithmetic operators preserve coercion of their operands, eg 1 + 1 is
# coercible to byte, but i + 1 is not coercible to byte for integer i


@dc.dataclass
class Operator(Expression):
    op_span: Span

    @abstractmethod
    def operate(self, *args):
        pass

    @property
    @abstractmethod
    def args(self):
        pass


@dc.dataclass
class Binary(Operator):
    left: Expression
    right: Expression

    @property
    def args(self):
        return self.left, self.right


@dc.dataclass
class Unary(Operator):
    arg: Expression

    @property
    def args(self):
        return self.arg,


class BooleanOp(Operator):
    type = DataType.BOOL

    def simplify(self):
        if all(isinstance(arg, PrimitiveValue) for arg in self.args):
            return BoolValue(
                bool(self.operate(*self.args)),
                self.span
            )

        return self


class LogicalOp(BooleanOp):
    def evaluate(self, env):
        return type(self)(
            self.op_span,
            *[arg.evaluate(env).cast(DataType.BOOL) for arg in self.args]
        ).simplify()


class CompareOp(BooleanOp):
    def evaluate(self, env):
        return type(self)(
            self.op_span,
            *[arg.evaluate(env).coerce(DataType.INT) for arg in self.args]
        ).simplify()


class EqualityOp(Binary, BooleanOp):
    def evaluate(self, env):
        left = self.left.evaluate(env)
        right = self.right.evaluate(env)

        if left.type == right.type == DataType.BOOL:
            return type(self)(self.op_span, left, right).simplify()

        return type(self)(
            self.op_span,
            left.coerce(DataType.INT),
            right.coerce(DataType.INT)
        ).simplify()


class ArithmeticOp(Operator):
    type = DataType.INT

    def simplify(self):
        if all(isinstance(arg, IntValue) for arg in self.args):
            return IntValue(
                int(self.operate(*self.args)),
                self.span, all(arg.literal for arg in self.args)
            )

        return self


class BinaryArithmeticOp(Binary, ArithmeticOp):
    def evaluate(self, env):
        left = self.left.evaluate(env)
        right = self.right.evaluate(env)
        result = type(self)(
            self.op_span,
            left.coerce(DataType.INT),
            right.coerce(DataType.INT)
        ).simplify()

        match (left, right):
            case ((Expression(type=DataType.BYTE), IntValue(literal=True)) |
                 (IntValue(literal=True), Expression(type=DataType.BYTE))):
                # goofiness to preserve literal status
                if result.coercible(DataType.BYTE):
                     return result.coerce(DataType.BYTE)
                return result.cast(DataType.BYTE)
        return result


class UnaryArithmeticOp(Unary, ArithmeticOp):
    def evaluate(self, env):
        arg = self.arg.evaluate(env)
        result = type(self)(
            self.op_span, arg.coerce(DataType.INT)
        )

        if arg.type == DataType.BYTE:
            if result.coercible(DataType.BYTE):
                return result.coerce(DataType.BYTE)
            return result.cast(DataType.BYTE)
        return result


class Add(BinaryArithmeticOp):
    operate = operator.add

class Sub(BinaryArithmeticOp):
    operate = operator.sub

class Mul(BinaryArithmeticOp):
    operate = operator.mul

class Div(BinaryArithmeticOp):
    def operate(self, left, right):
        try:
            return left // right
        except ZeroDivisionError:
            raise TypeCheckError('Division by zero', self.span)

class Mod(BinaryArithmeticOp):
    def operate(self, left, right):
        try:
            return left % right
        except ZeroDivisionError:
            raise TypeCheckError('Modulus of zero', self.span)

class Plus(UnaryArithmeticOp):
    operate = operator.pos

class Minus(UnaryArithmeticOp):
    operate = operator.neg

class And(Binary, LogicalOp):
    @staticmethod
    def operate(left, right):
        return bool(left and right)

class Or(Binary, LogicalOp):
    @staticmethod
    def operate(left, right):
        return bool(left or right)

class Not(Unary, LogicalOp):
    @staticmethod
    def operate(arg):
        return not arg

class Lt(Binary, CompareOp):
    operate = operator.lt

class Gt(Binary, CompareOp):
    operate = operator.gt

class LtE(Binary, CompareOp):
    operate = operator.le

class GtE(Binary, CompareOp):
    operate = operator.ge

class Eq(Binary, EqualityOp):
    operate = operator.eq

class Neq(Binary, EqualityOp):
    operate = operator.ne

# Is isn't considered a true operator, it's just an expression
@dc.dataclass(frozen=True)
class Is(Expression):
    op_span: Span
    expr: Expression
    type: DataType

    def evaluate(self, env):
        return self.expr.evaluate(env).cast(self.type)
