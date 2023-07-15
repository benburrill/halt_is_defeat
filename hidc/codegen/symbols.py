from .asm import Section
from hidc.ast import ArrayType, DataType, Ident
import dataclasses as dc
import enum

class AccessMode(enum.Enum):
    R = enum.auto()  # state constant
    RC = enum.auto() # const constant
    RW = enum.auto() # mutable

    @property
    def section(self):
        if self == AccessMode.RC:
            return Section.CONST
        return Section.STATE


@dc.dataclass(frozen=True)
class ConcreteArrayType:
    el_type: DataType
    access: AccessMode


ConcreteType = DataType | ConcreteArrayType

@dc.dataclass(frozen=True)
class ConcreteSignature:
    name: Ident
    concrete_params: tuple[ConcreteType, ...]

    @property
    def abstract_params(self):
        return tuple(
            # TODO: maybe property abstract_type on concrete types?
            #  (including DataType), maybe even on ArrayType as well
            t if isinstance(t, DataType)
            else ArrayType(t.el_type, t.access != AccessMode.RW)
            for t in self.concrete_params
        )
