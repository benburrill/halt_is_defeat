from . import abc
from hidc.lexer.tokens import DataType, Ident, Flavor

import dataclasses as dc
from collections import ChainMap
from collections.abc import Sequence


@dc.dataclass(frozen=True)
class ArrayType:
    el_type: DataType
    const: bool

    def __str__(self):
        if self.const:
            return f'const {self.el_type}[]'
        return f'{self.el_type}[]'


Type = ArrayType | DataType


@dc.dataclass(frozen=True)
class Variable:
    name: str
    type: Type
    const: bool


@dc.dataclass(frozen=True)
class UnresolvedName:
    name: str
    type = DataType.VOID
    const = True


class VarTable(ChainMap):
    @property
    def globals(self):
        return self.maps[-1]

    @property
    def is_global(self):
        return len(self.maps) <= 1



@dc.dataclass(frozen=True)
class FuncSignature:
    name: Ident
    arg_types: Sequence[Type]

    def __str__(self):
        params = ', '.join(map(str, self.arg_types))
        return f'{self.name.name}({params})'


@dc.dataclass(frozen=True)
class Environment:
    vars: VarTable
    funcs: dict
    return_type: DataType = None

    def new_child(self, return_type=None):
        return Environment(
            self.vars.new_child(), self.funcs,
            self.return_type if return_type is None else return_type
        )
