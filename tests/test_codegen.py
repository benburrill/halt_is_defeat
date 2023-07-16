from spasm.parser import Parser as SphinxParser
from spasm.context import ExecutionContext, VirtualContext
from spasm.emulator import Emulator

class RecorderContext(ExecutionContext):
    def __init__(self):
        super().__init__()
        self.reset()

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

def make_emulator(lines, args):
    ps = SphinxParser(args)
    ps.parse_lines(lines)
    return Emulator(
        ps.get_program(),
        vctx=VirtualContext(),
        rctx=RecorderContext()
    )

def run_to_flag(emulator, max_cycles=10000):
    initial_flags = len(emulator.rctx.flags)
    for _ in range(max_cycles):
        if len(emulator.rctx.flags) != initial_flags:
            return emulator
        if not emulator.step():
            return emulator
    assert False, 'max cycles exceeded'


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

    assert emulator.rctx.flags == ['win']
    assert list(emulator.rctx.bytes) == [3, 2, 1, 0]
