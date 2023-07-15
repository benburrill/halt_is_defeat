from . import abc
from hidc.lexer.tokens import DataType, Ident, Flavor
from hidc.lexer import Span

import dataclasses as dc
from collections import ChainMap
from collections.abc import Sequence
from hidc.errors import TypeCheckError


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
    type = DataType.EMPTY
    const = True


class VarTable(ChainMap):
    @property
    def globals(self):
        return self.maps[-1]

    @property
    def is_global(self):
        return len(self.maps) <= 1


@dc.dataclass(frozen=True)
class Environment:
    vars: VarTable
    funcs: dict[Ident, dict]
    options: dict = dc.field(default_factory=dict)
    return_type: DataType = None

    @classmethod
    def empty(cls, **options):
        return cls(VarTable(), {}, options)

    def new_child(self, return_type=None):
        return Environment(
            self.vars.new_child(), self.funcs, self.options,
            self.return_type if return_type is None else return_type
        )

    def add_funcs(self, func_defs):
        for func in func_defs:
            homonyms = self.funcs.setdefault(func.name, {})
            sig = func.param_types
            try:
                prev_def = homonyms[sig]
            except KeyError:
                homonyms[sig] = func
            else:
                raise TypeCheckError(
                    f'Redefinition of function {func.signature}',
                    (prev_def.span, func.span) if hasattr(prev_def, 'span')
                    else (func.span,)
                )
