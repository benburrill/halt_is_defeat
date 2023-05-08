from . import abc
from hidc.lexer.tokens import DataType, Ident
from hidc.errors import TypeCheckError

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
    def __setitem__(self, name, declaration):
        if prev_decl := self.get(name):
            # Fine to shadow globals, so long as we are not in the
            # global scope.
            if self.is_global or prev_decl is not self.globals.get(name):
                raise TypeCheckError(
                    f'Redeclaration of variable {name}',
                    declaration.span
                )

        super().__setitem__(name, declaration)

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
        return f'{self.name}({params})'


@dc.dataclass(frozen=True)
class Environment:
    vars: VarTable
    funcs: dict

    def new_child(self):
        return Environment(self.vars.new_child(), self.funcs)
