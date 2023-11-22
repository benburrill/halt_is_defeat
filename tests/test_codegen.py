from spasm.parser import Parser as SphinxParser
from spasm.context import ExecutionContext, VirtualContext
from spasm.emulator import Emulator

from hidc.lexer import SourceCode
from hidc.parser import parse
from hidc.ast import Environment
from hidc.codegen import CodeGen
from hidc.errors import CompilerError, CodeGenError

from pytest import raises

class RecorderContext(ExecutionContext):
    def __init__(self):
        super().__init__()
        self.reset()
        self.vctx = VirtualContext()

    def reset(self):
        self.flags = []
        self.bytes = bytearray()
        self.time_slept = 0

    def output(self, val):
        self.bytes += val[:1]

    def sleep(self, millis):
        self.time_slept += millis

    def on_flag(self, prog, flag):
        self.flags.append(flag)

    def virtualize(self):
        return self.vctx

def make_emulator(lines, args=()):
    ps = SphinxParser(args)
    ps.parse_lines(lines)
    return Emulator(
        ps.get_program(),
        ctx=RecorderContext()
    )

def run_to_flag(emulator, max_cycles=10000):
    initial_flags = len(emulator.ctx.flags)
    for _ in range(max_cycles):
        if len(emulator.ctx.flags) != initial_flags:
            return emulator
        if not emulator.step():
            return emulator
    assert False, 'max cycles exceeded'

def compile(source, word_size=2, stack_size=500, unchecked=False, **options):
    env = Environment.empty(**options)
    parse(SourceCode.from_string(source)).evaluate(env)
    cg = CodeGen(env, word_size=word_size, stack_size=stack_size, unchecked=unchecked)
    return list(cg.gen_lines())

def test_basic():
    emulator = run_to_flag(make_emulator([
        b'%argv <count>',
        b'%section state',
        b'counter: .arg count word',
        b'%section code',
        b'loop:',
        b'yield [counter]',
        b'sub [counter], [counter], 1',
        b'j loop',
        b'hge [counter], 0',
        b'flag win',
        b'tnt: j tnt',
        b'halt'
    ], ['3']))

    assert emulator.ctx.flags == ['win']
    assert list(emulator.ctx.bytes) == [3, 2, 1, 0]

def test_hello():
    prog = compile("""
    empty @is_you() {
        writeln("Hello world!");
    }
    """)

    emulator = run_to_flag(make_emulator(prog))
    assert emulator.ctx.flags == ['win']
    assert bytes(emulator.ctx.bytes) == b'Hello world!\n'

def test_ints():
    prog = compile("""
    empty @is_you() {
        writeln(0);
        writeln(1);
        writeln(-1);
        writeln(32767);
        writeln(-32768);
    }
    """)

    emulator = run_to_flag(make_emulator(prog))
    assert emulator.ctx.flags == ['win']
    assert bytes(emulator.ctx.bytes).splitlines(keepends=True) == [
        b'0\n',
        b'1\n',
        b'-1\n',
        b'32767\n',
        b'-32768\n'
    ]
