from .symbols import DataType
from .expressions import Expression, BoolValue, IntValue, PrimitiveValue
from hidc.lexer import Span
from hidc.lexer.tokens import OpToken
from hidc.errors import TypeCheckError

import dataclasses as dc
from hidc.utils.data_abc import Abstract
from abc import abstractmethod
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


class Operator(Expression):
    op_span: Abstract[Span]
    token: Abstract[OpToken]

    @abstractmethod
    def operate(self, *args):
        pass

    @property
    @abstractmethod
    def args(self):
        pass

binary_ops = {}
@dc.dataclass(frozen=True)
class Binary(Operator):
    op_span: Span
    left: Expression
    right: Expression

    @property
    def args(self):
        return self.left, self.right

    @property
    def span(self):
        return self.left.span | self.right.span

    def __init_subclass__(cls, **kwargs):
        if tok := cls.__dict__.get('token', None):
            if tok in binary_ops:
                raise TypeError('Duplicate token')
            binary_ops[tok] = cls


unary_ops = {}
@dc.dataclass(frozen=True)
class Unary(Operator):
    op_span: Span
    arg: Expression

    @property
    def args(self):
        return self.arg,

    @property
    def span(self):
        return self.op_span | self.arg.span

    def __init_subclass__(cls, **kwargs):
        if tok := cls.__dict__.get('token', None):
            if tok in unary_ops:
                raise TypeError('Duplicate token')
            unary_ops[tok] = cls


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


class EqualityOp(BooleanOp):
    def evaluate(self, env):
        args = [arg.evaluate(env) for arg in self.args]
        if all(arg.type == DataType.BOOL for arg in args):
            return type(self)(self.op_span, *args).simplify()

        return type(self)(
            self.op_span,
            *[arg.coerce(DataType.INT) for arg in args]
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
    token = OpToken.ADD
    operate = operator.add

class Sub(BinaryArithmeticOp):
    token = OpToken.SUB
    operate = operator.sub

class Mul(BinaryArithmeticOp):
    token = OpToken.MUL
    operate = operator.mul

class Div(BinaryArithmeticOp):
    token = OpToken.DIV
    def operate(self, left, right):
        try:
            return left // right
        except ZeroDivisionError:
            raise TypeCheckError('Division by zero', self.op_span)

class Mod(BinaryArithmeticOp):
    token = OpToken.MOD
    def operate(self, left, right):
        try:
            return left % right
        except ZeroDivisionError:
            raise TypeCheckError('Modulus of zero', self.op_span)

class Pos(UnaryArithmeticOp):
    token = OpToken.ADD
    operate = operator.pos

class Neg(UnaryArithmeticOp):
    token = OpToken.SUB
    operate = operator.neg

class And(Binary, LogicalOp):
    token = OpToken.AND
    @staticmethod
    def operate(left, right):
        return bool(left and right)

class Or(Binary, LogicalOp):
    token = OpToken.OR
    @staticmethod
    def operate(left, right):
        return bool(left or right)

class Not(Unary, LogicalOp):
    token = OpToken.NOT
    @staticmethod
    def operate(arg):
        return not arg

class Lt(Binary, CompareOp):
    token = OpToken.LT
    operate = operator.lt

class Gt(Binary, CompareOp):
    token = OpToken.GT
    operate = operator.gt

class Le(Binary, CompareOp):
    token = OpToken.LE
    operate = operator.le

class Ge(Binary, CompareOp):
    token = OpToken.GE
    operate = operator.ge

class Eq(Binary, EqualityOp):
    token = OpToken.EQ
    operate = operator.eq

class Ne(Binary, EqualityOp):
    token = OpToken.NE
    operate = operator.ne

# Is isn't considered a true operator, it's just an expression
@dc.dataclass(frozen=True)
class Is(Expression):
    span: Span
    expr: Expression
    type: DataType

    def evaluate(self, env):
        return self.expr.evaluate(env).cast(self.type)
