# from hidc.utils.data_abc import DataABC, Abstract
from hidc.errors import InternalCompilerError
from abc import ABC, abstractmethod
import dataclasses as dc
import typing as ty
import enum


def lines(directives):
    iterator = iter(directives)
    indent = 0
    while True:
        try:
            directive = next(iterator)
        except StopIteration as ret:
            return ret.value

        asm = list(directive.lines())
        if isinstance(directive, Metadata):
            indent += directive.data.get('add_indent', 0)

            if span := directive.data.get('span', None):
                asm.insert(0, f'; Starting {span.start}'.encode('utf-8'))

        for line in asm:
            yield b'    ' * indent + line


def _escape_bytes(data, quote):
    result = b''
    for byte in data:
        if byte in b'\\':
            result += b'\\\\'
        if byte in quote:
            result += b'\\' + bytes([byte])
        elif byte in b'\n':
            result += b'\\n'
        elif byte in b'\r':
            result += b'\\r'
        elif 0x20 <= byte <= 0x7e:
            result += bytes([byte])
        else:
            result += b'\\x' + f'{byte:02x}'.encode('utf-8')
    return result

_T = ty.TypeVar('_T')
InstrGen = ty.Generator['Instruction', None, _T]

class Accessor(ABC):
    @abstractmethod
    def get(self, r_out: 'LabelRef') -> InstrGen['AssemblyExpression']:
        # Returns an AssemblyExpression which may be used to reference
        # the value stored in the Accessor.  This may be written to the
        # word at r_out in state, but not necessarily.  Nothing else in
        # memory will be written to.
        yield from ()

    def to(self, r_out: 'LabelRef'):
        # For when you DO actually definitely want to write the value to
        # a particular address and not just get an AssemblyExpression.
        result = yield from self.get(r_out)
        # State.set doesn't do a mov if result == State(r_out)
        # so this won't write unless it's necessary
        yield from State(r_out).set(result)

    def set(self, source: 'AssemblyExpression'):
        # Write the value from the source into the accessor.
        yield from ()
        raise InternalCompilerError(f'{self.__class__.__name__} not writable')

    def access_byte(self) -> 'Accessor':
        # Transform to byte access if possible
        return self


class AssemblyExpression(Accessor):
    @abstractmethod
    def __bytes__(self):
        pass

    def get(self, r_out):
        yield from ()
        return self

@dc.dataclass(frozen=True)
class SpecialArg(AssemblyExpression):
    asm: bytes
    def __bytes__(self):
        return self.asm

class Immediate(AssemblyExpression):
    def access_byte(self):
        # No point in implementing, but sure why not...
        return SpecialArg(b'(' + bytes(self) + b') & 0xFF')

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

    def access_byte(self):
        return IntLiteral(self.data & 0xFF, self.is_byte)

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

    def set(self, source):
        if source != self:
            yield Mov(self.immed, source)

    def access_byte(self) -> 'Accessor':
        return StateByte(self.immed)

@dc.dataclass(frozen=True)
class Const(AssemblyExpression):
    immed: Immediate
    def __bytes__(self):
        return b'{' + bytes(self.immed) + b'}'

    def access_byte(self) -> 'Accessor':
        return ConstByte(self.immed)

@dc.dataclass(frozen=True)
class StateByte(Accessor):
    immed: Immediate
    def get(self, r_out: 'LabelRef') -> InstrGen['AssemblyExpression']:
        yield Lbs(r_out, self.immed)
        return State(r_out)

    def set(self, source: 'AssemblyExpression'):
        yield Sbs(self.immed, source)

@dc.dataclass(frozen=True)
class ConstByte(Accessor):
    immed: Immediate
    def get(self, r_out: 'LabelRef') -> InstrGen['AssemblyExpression']:
        yield Lbc(r_out, self.immed)
        return State(r_out)

@dc.dataclass(frozen=True)
class Indirect(Accessor):
    section: 'Section'
    base: AssemblyExpression
    offset: AssemblyExpression = IntLiteral(0)
    def get(self, r_out: 'LabelRef') -> InstrGen['AssemblyExpression']:
        yield self.section.lwo(r_out, self.base, self.offset)
        return State(r_out)

    def set(self, source: 'AssemblyExpression'):
        yield self.section.swo(self.base, self.offset, source)

    def access_byte(self) ->'Accessor':
        return IndirectByte(self.section, self.base, self.offset)

@dc.dataclass(frozen=True)
class IndirectByte(Accessor):
    section: 'Section'
    base: AssemblyExpression
    offset: AssemblyExpression = IntLiteral(0)

    def get(self, r_out: 'LabelRef') -> InstrGen['AssemblyExpression']:
        yield self.section.lbo(r_out, self.base, self.offset)
        return State(r_out)

    def set(self, source: 'AssemblyExpression'):
        yield self.section.sbo(self.base, self.offset, source)

class VoidAccessor(Accessor):
    def get(self, r_out: 'LabelRef') -> InstrGen['AssemblyExpression']:
        yield from ()
        raise InternalCompilerError('Value was void')


class Directive(ABC):
    @abstractmethod
    def lines(self):
        yield from ()

@dc.dataclass
class Label(Directive):
    label: LabelRef
    def lines(self):
        yield bytes(self.label) + b':'

@dc.dataclass
class Metadata(Directive):
    data: dict = dc.field(init=False)

    def __init__(self, comment=None, **data):
        if comment is not None:
            data['comment'] = comment
        self.data = data

    def lines(self):
        if 'comment' in self.data:
            for line in self.data['comment'].splitlines():
                yield b'; ' + line.encode('utf-8')

class Instruction(Directive):
    code: ty.ClassVar[bytes]

    @property
    def args(self):
        return tuple(getattr(self, field.name) for field in dc.fields(self))

    def lines(self):
        if not self.args:
            yield self.code
        else:
            yield self.code + b' ' + b', '.join(map(bytes, self.args))

@dc.dataclass
class DestInstruction(Instruction):
    dest: AssemblyExpression

@dc.dataclass
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

@dc.dataclass
class Halt(Instruction):
    code = b'halt'

@dc.dataclass
class ConditionalHalt(Instruction):
    left: AssemblyExpression
    right: AssemblyExpression

class Heq(ConditionalHalt):
    code = b'heq'

class Hne(ConditionalHalt):
    code = b'hne'

class Hlt(ConditionalHalt):
    code = b'hlt'

class Hltu(ConditionalHalt):
    code = b'hltu'

class Hgt(ConditionalHalt):
    code = b'hgt'

class Hgtu(ConditionalHalt):
    code = b'hgtu'

class Hle(ConditionalHalt):
    code = b'hle'

class Hleu(ConditionalHalt):
    code = b'hleu'

class Hge(ConditionalHalt):
    code = b'hge'

class Hgeu(ConditionalHalt):
    code = b'hgeu'

@dc.dataclass
class Jump(Instruction):
    code = b'j'
    addr: AssemblyExpression

@dc.dataclass
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

@dc.dataclass
class Mov(ImmedDestInstruction):
    code = b'mov'
    value: AssemblyExpression

# TODO: Possibly make offset an optional argument of LoadInstruction
#  which generates appropriate LoadOffsetInstruction or something rather
#  than separating.  Same for StoreInstruction, but that's more awkward.
@dc.dataclass
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

@dc.dataclass
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

@dc.dataclass
class StoreInstruction(DestInstruction):
    value: AssemblyExpression

class Sws(StoreInstruction):
    code = b'sws'

class Sbs(StoreInstruction):
    code = b'sbs'

@dc.dataclass
class StoreOffsetInstruction(DestInstruction):
    offset: AssemblyExpression
    value: AssemblyExpression

class Swso(StoreOffsetInstruction):
    code = b'swso'

class Sbso(StoreOffsetInstruction):
    code = b'sbso'

@dc.dataclass
class Yield(Instruction):
    code = b'yield'
    output: AssemblyExpression

@dc.dataclass
class Sleep(Instruction):
    code = b'sleep'
    duration: AssemblyExpression

@dc.dataclass
class Flag(Instruction):
    code = b'flag'
    flag: SpecialArg


@dc.dataclass
class AsciiDirective(Directive):
    data: bytes

    def lines(self):
        yield b'.ascii "' + _escape_bytes(self.data, b'"') + b'"'

@dc.dataclass
class WordDirective(Directive):
    items: tuple[Immediate, ...] = dc.field(init=False)

    def __init__(self, *items: Immediate):
        self.items = items

    def lines(self):
        if len(self.items) >= 1:
            yield b'.word ' + b', '.join(bytes(word) for word in self.items)

@dc.dataclass
class ByteDirective(Directive):
    items: tuple[Immediate, ...] = dc.field(init=False)

    def __init__(self, *items: Immediate):
        self.items = items

    def lines(self):
        if len(self.items) >= 1:
            yield b'.byte ' + b', '.join(bytes(byte) for byte in self.items)

@dc.dataclass
class ZeroDirective(Directive):
    size: Immediate

    @classmethod
    def bytes(cls, size):
        return cls(IntLiteral(size))

    @classmethod
    def words(cls, size):
        return cls(WordOffset(size))

    def lines(self):
        yield b'.zero ' + bytes(self.size)

@dc.dataclass
class ArgDirective(Directive):
    var_name: str
    format: str
    params: tuple[str, ...] = ()

    def lines(self):
        yield (b'.arg ' + b' '.join([
            p.encode('utf-8')
            for p in (self.var_name, self.format, *self.params)
        ]))


def _store_const(*_):
    raise InternalCompilerError('Cannot store in const section')

_T_word = ty.Callable[[Immediate], AssemblyExpression]
_T_byte = ty.Callable[[Immediate], Accessor]
_T_l = ty.Callable[[Immediate, AssemblyExpression], LoadInstruction]
_T_lo = ty.Callable[[Immediate, AssemblyExpression, AssemblyExpression], LoadOffsetInstruction]
_T_s = ty.Callable[[AssemblyExpression, AssemblyExpression], StoreInstruction]
_T_so = ty.Callable[[AssemblyExpression, AssemblyExpression, AssemblyExpression], StoreOffsetInstruction]

class Section(enum.Enum):
    CONST = (Const, ConstByte, Lwc, Lwco, Lbc, Lbco, _store_const, _store_const, _store_const, _store_const)
    STATE = (State, StateByte, Lws, Lwso, Lbs, Lbso, Sws, Swso, Sbs, Sbso)

    def __init__(self, word: _T_word, byte: _T_byte,
                 lw: _T_l, lwo: _T_lo, lb: _T_l, lbo: _T_lo,
                 sw: _T_s, swo: _T_so, sb: _T_s, sbo: _T_so):
        self.word = word
        self.byte = byte
        self.lw = lw
        self.lwo = lwo
        self.lb = lb
        self.lbo = lbo
        self.sw = sw
        self.swo = swo
        self.sb = sb
        self.sbo = sbo
