from . import abc
from hidc.lexer.tokens import DataType, Ident
from hidc.errors import TypeCheckError

import enum
import dataclasses as dc
from collections.abc import Sequence

@dc.dataclass(frozen=True)
class DataType:
    token: Type


class Access(enum.Flag):
    RD = 0  # Readable
    RW = 1  # Read/Write
    RC = 2  # Stored in const section

@dc.dataclass(frozen=True)
class ArrayType:
    el_type: DataType
    access: Access = Access.RD

Type = ArrayType | DataType


@dc.dataclass(frozen=True)
class Variable:
    const: bool
    type: Type
    name: str


@dc.dataclass(frozen=True)
class FuncSignature:
    name: Ident
    params: Sequence[Variable]
