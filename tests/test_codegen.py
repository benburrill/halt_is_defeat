from spasm.parser import Parser as SphinxParser
from spasm.context import ExecutionContext, VirtualContext
from spasm.emulator import Emulator

from hidc.lexer import SourceCode
from hidc.parser import parse
from hidc.ast import Environment
from hidc.codegen import CodeGen
from hidc.errors import CompilerError, CodeGenError

import pytest
from pytest import raises

import pathlib


utils = """
empty write_arr(const bool[] arr) {
    write('[');
    for (int i = 0; i < arr.length; i += 1) {
        if (i != 0) { write(", "); }
        write(arr[i]);
    }
    write(']');
}

empty write_arr(const int[] arr) {
    write('[');
    for (int i = 0; i < arr.length; i += 1) {
        if (i != 0) { write(", "); }
        write(arr[i]);
    }
    write(']');
}
"""


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

def run_to_flag(emulator, expected_flag=None, *, max_cycles=100000):
    initial_flags = len(emulator.ctx.flags)
    for _ in range(max_cycles):
        if len(emulator.ctx.flags) != initial_flags:
            if expected_flag is not None:
                flag = emulator.ctx.flags[initial_flags]
                assert flag == expected_flag, f'unexpected flag {flag}'
            return bytes(emulator.ctx.bytes)
        if not emulator.step():
            assert False, 'program halted'
    assert False, 'max cycles exceeded'

def compile(source, word_size=2, stack_size=500, unchecked=False, **options):
    env = Environment.empty(**options)
    parse(SourceCode.from_string(source)).evaluate(env)
    cg = CodeGen(env, word_size=word_size, stack_size=stack_size, unchecked=unchecked)
    return list(cg.gen_lines())

def test_basic():
    emulator = make_emulator([
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
    ], ['3'])
    assert list(run_to_flag(emulator, 'win')) == [3, 2, 1, 0]

def test_hello():
    emulator = make_emulator(compile("""
    empty @is_you() {
        writeln("Hello world!");
    }
    """))

    assert run_to_flag(emulator, 'win') == b'Hello world!\n'

def test_ints():
    emulator = make_emulator(compile("""
        empty @is_you() {
            writeln(0);
            writeln(1);
            writeln(-1);
            writeln(32767);
            writeln(-32768);
            writeln(32768);
        }
    """))

    assert run_to_flag(emulator, 'win').splitlines(keepends=True) == [
        b'0\n',
        b'1\n',
        b'-1\n',
        b'32767\n',
        b'-32768\n',
        b'-32768\n'
    ]

@pytest.mark.parametrize('bools', [
    [],
    [True],
    [False],
    [True, True, False, True],
    [True, False, True, False, True, False, True, False, False, False,
     True, True, False, True, True, True, True, True, True, False, True,
     True, False, False, False, True, False, False, True, False, True]
])
def test_bool_array_literal(bools):
    result = '[' + ', '.join('true' if x else 'false' for x in bools) + ']'
    emulator = make_emulator(compile(utils + f"""
        const bool[] const_global = {result};
        bool[] mut_global = {result};
        empty @is_you() {{
            const bool[] const_local = {result};
            bool[] mut_local = {result};
            write_arr(const_global);
            write_arr(mut_global);
            write_arr(const_local);
            write_arr(mut_local);
        }}
    """))

    assert run_to_flag(emulator, 'win') == result.encode('utf-8') * 4


@pytest.mark.parametrize('length, positions', [
    (1, [0]),
    (5, [1, 4]),
    (8, [0, 3, 6]),
    (25, [3, 4, 6, 7, 10, 14, 21, 23]),
])
def test_bool_array_literal_complex_expr(length, positions):
    source = '[' + ', '.join(
        f'f({i})' if i in positions else 'false'
        for i in range(length)
    ) + ']'
    result = '[' + ', '.join(
        f'true' if i in positions else 'false'
        for i in range(length)
    ) + ']'

    emulator = make_emulator(compile(utils + f"""
        bool f(int i) {{
            // Should not interfere with array being built by caller
            int[] x = [i, 2*i, 3*i];
            writeln(i);
            return true;
        }}

        empty @is_you() {{
            bool[] arr = {source};
            write_arr(arr);
        }}
    """))

    assert run_to_flag(emulator, 'win') == (
        '\n'.join(map(str, positions)) + '\n' + result
    ).encode('utf-8')


def compile_example(filename, *args, **kwargs):
    test_dir = pathlib.Path(__file__).resolve().parent
    with open(test_dir.parent / 'examples' / filename) as f:
        return compile(f.read(), *args, **kwargs)

@pytest.fixture(scope='session')
def mergesort_example():
    return compile_example('mergesort.hid')

@pytest.mark.parametrize('array', [
    (),
    (1,),
    (5, 2, 8),
    (10, 44, 33, 67, 23, 22, 92, 87, 85, 3, 12, 29, 51),
    (2, 43, 82, 74, 6, 8, 77, 40, 38, 47, 92, 30, 9, 85, 94, 93),
])
def test_mergesort(mergesort_example, array):
    emulator = make_emulator(mergesort_example, tuple(map(str, array)))
    run_to_flag(emulator, 'progress')
    assert run_to_flag(emulator, 'win') == (
        f'Sorted: {sorted(array)}\n'.encode('utf-8')
    )

def test_try_stop_func_call():
    emulator = make_emulator(compile("""
        empty !f() {
            writeln("!f()");
            !is_defeat();
        }
    
        empty !g() {
            writeln("!g()");
            !f();
        }
    
        empty @is_you() {
            try {
                writeln("try");
                !g();
            } stop {
                writeln("stop");
            }
        }
    """))

    assert run_to_flag(emulator, 'win') == (
        b'try\n'
        b'!g()\n'
        b'!f()\n'
        b'stop\n'
    )

def test_int_array_inc_assign():
    emulator = make_emulator(compile(utils + """
        empty f(int[] arr) {
            arr[0] += 1;
            arr[0] *= 3;
            arr[0] += arr[0];
            arr[1] -= arr[0];
        }
    
        empty @is_you() {
            int[] arr = [2, 3, 6];
            f(arr);
            write_arr(arr);
        }
    """))

    assert run_to_flag(emulator, 'win') == b'[18, -15, 6]'

def test_bool_array_assign():
    emulator = make_emulator(compile(utils + """
        empty f(bool[] arr) {
            arr[1] = arr[0];
            arr[2] = false;
            arr[3] = not arr[3];
        }

        empty @is_you() {
            bool[] arr = [true, false, true, false];
            f(arr);
            write_arr(arr);
        }
    """))

    assert run_to_flag(emulator, 'win') == b'[true, true, false, true]'


def test_array_ref():
    emulator = make_emulator(compile(utils + """
        empty @is_you() {
            int[] x = [1, 2, 3];
            write_arr(x);
            {
                int[] y = x;
                y[0] = 5;
                write_arr(y);
            }
            {
                // Test that y going out of scope didn't deallocate x
                // (z should not overwrite x).
                int[] z = [0, 0, 0];
                write_arr(z);
            }
            write_arr(x);
        }
    """))

    assert run_to_flag(emulator, 'win') == (
        b'[1, 2, 3]'
        b'[5, 2, 3]'
        b'[0, 0, 0]'
        b'[5, 2, 3]'
    )

def test_stack_overflow_array_literal():
    emulator = make_emulator(compile("""
        empty f() {
            writeln("f()");
            int[] x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
            writeln("Allocated x");
            h(); // Simply calling a function should be fine here,
            g(); // but not calling a function that allocates too much
        }
        empty g() {
            writeln("g()");
            int[] y = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
            writeln("Allocated y");
        }
        empty h() {
            writeln("h()");
        }
        empty @is_you() {
            f();
        }
    """, stack_size=20))

    assert run_to_flag(emulator, 'stack_overflow') == (
        b'f()\n'
        b'Allocated x\n'
        b'h()\n'
    )

def test_deallocate_array_literal():
    emulator = make_emulator(compile("""
        empty f() {
            writeln("f()");
            // Now the array only exists for this scope
            { int[] x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]; }
            writeln("Allocated x");
            h();
            g();
        }
        empty g() {
            writeln("g()");
            int[] y = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
            writeln("Allocated y");
        }
        empty h() {
            writeln("h()");
        }
        empty @is_you() {
            f();
        }
    """, stack_size=20))

    assert run_to_flag(emulator, 'win') == (
        b'f()\n'
        b'Allocated x\n'
        b'h()\n'
        b'g()\n'
        b'Allocated y\n'
    )

def test_early_stack_overflow_local_array_literals():
    emulator = make_emulator(compile("""
        empty @is_you() {
            writeln("Before");
            int[] x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
            int[] y = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
            writeln("After");
        }
    """, stack_size=20))

    assert run_to_flag(emulator, 'stack_overflow') == b''

def test_deallocate_local_array_literal():
    emulator = make_emulator(compile("""
        empty @is_you() {
            writeln("Before");
            { int[] x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]; }
            { int[] y = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]; }
            writeln("After");
        }
    """, stack_size=20))

    assert run_to_flag(emulator, 'win') == b'Before\nAfter\n'

def test_deallocate_local_array_literal_func_call():
    emulator = make_emulator(compile(utils + """
        empty f(int[] arr) {
            write_arr(arr);
            // Test that array literals passed to this function were not
            // deallocated early (this array should not overwrite them).
            int[] x = [0, 0, 0];
            write_arr(x);
            write_arr(arr);
            write_arr(x); // Just for convenience writing the assertion
        }
        empty @is_you() {
            f([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
            f([12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]);
            int[] x = [23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33];
            f(x);
        }
    """))

    assert run_to_flag(emulator, 'win') == (
        b'[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11][0, 0, 0]' * 2 +
        b'[12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22][0, 0, 0]' * 2 +
        b'[23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33][0, 0, 0]' * 2
    )

def test_stack_overflow_vla():
    emulator = make_emulator(compile("""
        empty @is_you() {
            writeln("Start");
            int x[11];
            writeln("Allocated x");
            int y[11];
            writeln("Allocated y");
        }
    """, stack_size=20))

    assert run_to_flag(emulator, 'stack_overflow') == (
        b'Start\n'
        b'Allocated x\n'
    )

def test_deallocate_vla():
    emulator = make_emulator(compile("""
        empty @is_you() {
            writeln("Start");
            { int x[11]; }
            writeln("Allocated and deallocated x");
            { int y[11]; }
            writeln("Allocated and deallocated y");
        }
    """, stack_size=20))

    assert run_to_flag(emulator, 'win') == (
        b'Start\n'
        b'Allocated and deallocated x\n'
        b'Allocated and deallocated y\n'
    )
