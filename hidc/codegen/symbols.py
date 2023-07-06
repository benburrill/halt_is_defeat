from .asm import Section
from hidc.lexer.tokens import DataType, Ident
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
    arg_types: tuple[ConcreteType, ...]
