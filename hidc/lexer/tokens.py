import enum
import dataclasses as dc


class Token:
    pass


@dc.dataclass(frozen=True)
class StringToken(Token):
    data: bytes


@dc.dataclass(frozen=True)
class IntToken(Token):
    data: int


@dc.dataclass(frozen=True)
class CharToken(Token):
    data: int


# These are the identifier flavors, ie Baba Is You properties
# Flavor is not Token
class Flavor(enum.Enum):
    NONE = ''
    YOU = '@'
    DEFEAT = '!'

    def __str__(self):
        return self.value


@dc.dataclass(frozen=True)
class Ident(Token):
    base_name: str
    flavor: Flavor = Flavor.NONE

    @classmethod
    def you(cls, name):
        return cls(name, Flavor.YOU)

    @classmethod
    def defeat(cls, name):
        return cls(name, Flavor.DEFEAT)

    @property
    def name(self):
        return str(self.flavor) + self.base_name

    def __repr__(self):
        return f'<IdentToken {self.name}>'

    def __str__(self):
        return self.name


class EnumToken(Token, enum.Enum):
    # Python 3.11 decided to completely change how Enums with mixins
    # work in a very bizarre way.  The best approach I can find to make
    # enums with mixins work consistently in Python 3.10 and 3.11 is to
    # define a __new__ that sets _value_ on the newly created member.
    # See my rant here: https://discuss.python.org/t/inconsistent-behavior-with-python-3-11-enum-mixin-class-behavior/24613
    def __new__(cls, val):
        member = Token.__new__(cls)
        member._value_ = val
        return member

    def __str__(self):
        return self.value


enum_tokens = set()
def include_enum(cls):
    enum_tokens.update(cls)
    return cls


@include_enum
class OpToken(EnumToken):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'
    MOD = '%'
    EQ = '=='
    NE = '!='
    LT = '<'
    GT = '>'
    LE = '<='
    GE = '>='
    OR = 'or'
    AND = 'and'
    NOT = 'not'
    IS = 'is'


@include_enum
class IncAssignToken(EnumToken):
    IADD = '+='
    ISUB = '-='
    IMUL = '*='
    IDIV = '/='
    IMOD = '%='

    @property
    def operator(self):
        return OpToken(self.value[:-1])


@include_enum
class StmtToken(EnumToken):
    ASSIGN = '='
    BREAK = 'break'
    CONTINUE = 'continue'
    RETURN = 'return'
    CONST = 'const'


@include_enum
class SepToken(EnumToken):
    SEMICOLON = ';'
    COMMA = ','
    # DOT feels a bit out of place here
    DOT = '.'


@include_enum
class BracToken(EnumToken):
    LPAREN = '('
    RPAREN = ')'
    LCURLY = '{'
    RCURLY = '}'
    LSQUARE = '['
    RSQUARE = ']'

    @property
    def pair(self):
        # goofy and probably pointless... but why not?
        alt_name = 'RL'['LR'.index(self.name[0])] + self.name[1:]
        return type(self)[alt_name]


@include_enum
class BlockToken(EnumToken):
    IF = 'if'
    ELSE = 'else'
    WHILE = 'while'
    FOR = 'for'
    TRY = 'try'
    UNDO = 'undo'
    STOP = 'stop'
    PREEMPT = 'preempt'


@include_enum
class DataType(EnumToken):
    INT = 'int'
    BOOL = 'bool'
    BYTE = 'byte'
    STRING = 'string'
    VOID = 'void'

    @property
    def byte_sized(self):
        return self in {DataType.BYTE, DataType.BOOL}


@include_enum
class BoolToken(EnumToken):
    TRUE = 'true'
    FALSE = 'false'

    @property
    def data(self):
        return self != type(self).FALSE
