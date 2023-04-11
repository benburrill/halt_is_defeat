from hidc.lexer.tokens import Type, Ident

import dataclasses as dc
from collections.abc import Sequence

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


@dc.dataclass(frozen=True)
class FuncSignature:
    name: Ident
    params: Sequence[Variable]
