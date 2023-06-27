# from hidc.utils.data_abc import DataABC, Abstract
from abc import ABC, abstractmethod
import dataclasses as dc
from typing import ClassVar


def _escape_bytes(data, quote):
    result = b''
    for byte in data:
        if byte in b'\\':
            result += b'\\\\'
        if byte in quote:
            result += b'\\' + bytes([byte])
        elif byte in b'\n':
            result += b'\\n'
        elif byte in b'\\r':
            result += b'\\r'
        elif 0x20 <= byte <= 0x7e:
            result += bytes([byte])
        else:
            result += b'\\x' + f'{byte:02x}'.encode('utf-8')
    return result


class AssemblyExpression(ABC):
    @abstractmethod
    def __bytes__(self):
        pass

@dc.dataclass(frozen=True)
class SpecialArg(AssemblyExpression):
    asm: bytes
    def __bytes__(self):
        return self.asm

class Immediate(AssemblyExpression):
    pass

@dc.dataclass(frozen=True)
class LabelRef(Immediate):
    label_name: str
    def __bytes__(self):
        return self.label_name.encode('utf-8')

@dc.dataclass(frozen=True)
class IntLiteral(Immediate):
    data: int
    is_byte: bool = dc.field(default=False, compare=False)

    def __bytes__(self):
        if self.is_byte and 0 <= self.data <= 255:
            return b"'" + _escape_bytes(bytes([self.data]), b"'") + b"'"
        return str(self.data).encode('utf-8')

@dc.dataclass(frozen=True)
class WordOffset(Immediate):
    words: int
    def __bytes__(self):
        return str(self.words).encode('utf-8') + b'w'

@dc.dataclass(frozen=True)
class State(AssemblyExpression):
    immed: Immediate
    def __bytes__(self):
        return b'[' + bytes(self.immed) + b']'

@dc.dataclass(frozen=True)
class Const(AssemblyExpression):
    immed: Immediate
    def __bytes__(self):
        return b'{' + bytes(self.immed) + b'}'

class Directive(ABC):
    @abstractmethod
    def __bytes__(self):
        pass

@dc.dataclass(frozen=True)
class Label(Directive):
    label: LabelRef
    def __bytes__(self):
        return bytes(self.label) + b':'

@dc.dataclass(frozen=True)
class Comment(Directive):
    text: str
    def __bytes__(self):
        return b'\n'.join(
            b'; ' + line.encode('utf-8')
            for line in self.text.splitlines()
        )


# TODO: indent will probably need to be stored in the instructions
#  themselves after all :(
#  Or maybe directives for increase indent, decrease indent, which would
#  be similar to Comment
#  A peephole optimizer would ignore both comments and indent directives
class Instruction(Directive):
    code: ClassVar[bytes]

    @property
    def args(self):
        return tuple(getattr(self, field.name) for field in dc.fields(self))

    def __bytes__(self):
        return b' '.join([self.code, *map(bytes, self.args)])

@dc.dataclass(frozen=True)
class DestInstruction(Instruction):
    dest: AssemblyExpression

@dc.dataclass(frozen=True)
class ImmedDestInstruction(DestInstruction):
    dest: Immediate
    @property
    def args(self):
        fields = dc.fields(self)
        assert fields[0].name == 'dest'
        return (
            State(self.dest),
            *[getattr(self, field.name) for field in fields[1:]]
        )

@dc.dataclass(frozen=True)
class Halt(Instruction):
    code = b'halt'

@dc.dataclass(frozen=True)
class ConditionalHalt(Instruction):
    left: AssemblyExpression
    right: AssemblyExpression

class Heq(ConditionalHalt):
    code = b'heq'

class Hne(ConditionalHalt):
    code = b'hne'

class Hlt(ConditionalHalt):
    code = b'hlt'

class Hgt(ConditionalHalt):
    code = b'hgt'

class Hle(ConditionalHalt):
    code = b'hle'

class Hge(ConditionalHalt):
    code = b'hge'

@dc.dataclass(frozen=True)
class Jump(Instruction):
    code = b'j'
    addr: AssemblyExpression

@dc.dataclass(frozen=True)
class ArithmeticInstruction(ImmedDestInstruction):
    left: AssemblyExpression
    right: AssemblyExpression

class Add(ArithmeticInstruction):
    code = b'add'

class Sub(ArithmeticInstruction):
    code = b'sub'

class Mul(ArithmeticInstruction):
    code = b'mul'

class Div(ArithmeticInstruction):
    code = b'div'

class Mod(ArithmeticInstruction):
    code = b'mod'

class And(ArithmeticInstruction):
    code = b'and'

class Or(ArithmeticInstruction):
    code = b'or'

class Xor(ArithmeticInstruction):
    code = b'xor'

class Asl(ArithmeticInstruction):
    code = b'asl'

class Asr(ArithmeticInstruction):
    code = b'asr'

@dc.dataclass(frozen=True)
class Mov(ImmedDestInstruction):
    code = b'mov'
    value: AssemblyExpression

@dc.dataclass(frozen=True)
class LoadInstruction(ImmedDestInstruction):
    source: AssemblyExpression

class Lws(LoadInstruction):
    code = b'lws'

class Lwc(LoadInstruction):
    code = b'lwc'

class Lbs(LoadInstruction):
    code = b'lbs'

class Lbc(LoadInstruction):
    code = b'lbc'

@dc.dataclass(frozen=True)
class LoadOffsetInstruction(ImmedDestInstruction):
    source: AssemblyExpression
    offset: AssemblyExpression

class Lwso(LoadOffsetInstruction):
    code = b'lwso'

class Lwco(LoadOffsetInstruction):
    code = b'lwco'

class Lbso(LoadOffsetInstruction):
    code = b'lbso'

class Lbco(LoadOffsetInstruction):
    code = b'lbco'

@dc.dataclass(frozen=True)
class StoreInstruction(DestInstruction):
    value: AssemblyExpression

class Sws(StoreInstruction):
    code = b'sws'

class Sbs(StoreInstruction):
    code = b'sbs'

@dc.dataclass(frozen=True)
class StoreOffsetInstruction(DestInstruction):
    offset: AssemblyExpression
    value: AssemblyExpression

class Swso(StoreOffsetInstruction):
    code = b'swso'

class Sbso(StoreOffsetInstruction):
    code = b'sbso'

@dc.dataclass(frozen=True)
class Yield(Instruction):
    code = b'yield'
    output: AssemblyExpression

@dc.dataclass(frozen=True)
class Sleep(Instruction):
    code = b'sleep'
    duration: AssemblyExpression

@dc.dataclass(frozen=True)
class Flag(Instruction):
    code = b'flag'
    flag: SpecialArg


@dc.dataclass(frozen=True)
class AsciiDirective(Directive):
    data: bytes

    def __bytes__(self):
        return b'.ascii "' + _escape_bytes(self.data, b'"') + b'"'

# TODO: support multiple words/bytes in single directive
@dc.dataclass(frozen=True)
class WordDirective(Directive):
    word: Immediate

    def __bytes__(self):
        return b'.word ' + bytes(self.word)

@dc.dataclass(frozen=True)
class ByteDirective(Directive):
    byte: Immediate

    def __bytes__(self):
        return b'.byte ' + bytes(self.byte)

@dc.dataclass(frozen=True)
class ZeroDirective(Directive):
    size: Immediate

    @classmethod
    def bytes(cls, size):
        return cls(IntLiteral(size))

    @classmethod
    def words(cls, size):
        return cls(WordOffset(size))

    def __bytes__(self):
        return b'.zero ' + bytes(self.size)