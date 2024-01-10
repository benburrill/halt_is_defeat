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

empty write_arr(const byte[] arr) {
    write('[');
    for (int i = 0; i < arr.length; i += 1) {
        if (i != 0) { write(", "); }
        write("0x");
        write("0123456789ABCDEF"[(arr[i] / 0x10) % 0x10]);
        write("0123456789ABCDEF"[arr[i] % 0x10]);
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

def test_command_line_args():
    emulator = make_emulator(compile(utils + """
        empty @is_you(const string[] args) {
            for (int i = 0; i < args.length; i += 1) {
                write("> ");
                writeln(args[i]);
            }
        }
    """), ['Hello world', '', 'potato'])

    assert run_to_flag(emulator, 'win') == (
        b'> Hello world\n'
        b'> \n'
        b'> potato\n'
    )

def test_mixed_command_line():
    emulator = make_emulator(compile(utils + """
        empty @is_you(string s, int[] i, byte b) {
            write(s);
            writeln(b);
            write_arr(i);
        }
    """), ['Hello', '1', '2', '3', '4', '5', '33'])

    assert run_to_flag(emulator, 'win') == (
        b'Hello!\n'
        b'[1, 2, 3, 4, 5]'
    )

def test_entry_point_errors():
    with raises(CodeGenError, match='Level is empty'):
        compile("empty is_you() {}")
    with raises(CodeGenError, match='must return empty'):
        compile("int @is_you() { return 0; }")
    with raises(CodeGenError, match='text on text is weak'):
        compile("""
            empty @is_you() {}
            empty @is_you(const string[] args) {}
        """)
    with raises(CodeGenError, match='must be const'):
        compile("empty @is_you(string[] args) {}")
    with raises(CodeGenError, match='more than one array'):
        compile("empty @is_you(const string[] a, int[] b) {}")

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
            // We don't have to worry about compiler optimizing these to
            // global arrays because f() takes non-const array. 
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

def test_array_index_array():
    emulator = make_emulator(compile("""
        empty @is_you(int x, int y) {
            // We use arguments here to suppress compiler optimizations
            write([x, 12, 47][[1, y, 2][1]]);
        }
    """), ['5', '0'])

    assert run_to_flag(emulator, 'win') == b'5'

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

@pytest.mark.parametrize('cont', [0, 1])
def test_loop_deallocate_array(cont):
    emulator = make_emulator(compile("""
        empty @is_you(int cont) {
            for (int i = 0; i < 20; i += 1) {
                int arr[11];
                arr[0] = i;
                writeln(arr[0]);
                // Should work the same regardless of whether we
                // continue early or not.
                if (cont) { continue; }
                write("");
            }
        }
    """, stack_size=20), [cont])

    assert run_to_flag(emulator, 'win') == b''.join(
        str(i).encode('utf-8') + b'\n'
        for i in range(20)
    )

def test_push_byte_to_int():
    emulator = make_emulator(compile("""
        empty f(int i) {
            writeln(i);
        }

        empty @is_you() {
            byte b = 0x42;
            int i = 0x1111;
            f(i);
            f(b);
        }
    """))

    assert run_to_flag(emulator, 'win') == b'4369\n66\n'

def test_string_as_byte_array():
    emulator = make_emulator(compile(utils + """
        empty @is_you() {
            string s = "Hello!";
            write_arr(s);
        }
    """))

    assert run_to_flag(emulator, 'win') == (
        b'[0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x21]'
    )

def test_speculation():
    emulator = make_emulator(compile("""
        int f(int i) {
            writeln(i);
            return 42;
        }

        empty @is_you() {
            writeln(f(0) ?? 41);
            writeln(f(1) ?? 42);
        }
    """))

    assert run_to_flag(emulator, 'win') == (
        b'0\n42\n'
        b'42\n'
    )

def test_uninitialized_bool():
    # Some HiD code, such as the SAT solver assumes that uninitialized
    # arrays of bools will consist of valid bool values.
    # Although uninitialized array elements have unspecified value, I
    # only consider it as undefined behavior to "do things" with these
    # values in the case of string arrays.
    # Here we test that not and == work correctly on uninitialized bools
    #
    # We will also assume that values are obtained from the bits of the
    # previous array, though this is not strictly required behavior, so
    # the test could be changed to simply check that there are no X's.
    emulator = make_emulator(compile(utils + """
        empty @is_you() {
            {
                byte[] x = [0xAA, 0xAA];
                write_arr(x);
            }
            bool arr[10];
            for (int i = 0; i < arr.length; i += 1) {
                if (arr[i] == true) {
                    bool n = not arr[i];
                    if (n == false) {
                        write('1');
                    } else {
                        write('X');
                    }
                } else if (arr[i] == false) {
                    bool n = not arr[i];
                    if (n == true) {
                        write('0');
                    } else {
                        write('X');
                    }
                } else {
                    write('X');
                }
            }
        }
    """))

    assert run_to_flag(emulator, 'win') == b'[0xAA, 0xAA]' + b'01' * 5

def test_bool_casts():
    emulator = make_emulator(compile("""
        empty @is_you() {
            int i0 = 0;
            int i1 = 42;
            string s0 = "";
            string s1 = "Hello";
            int[] a0 = [];
            int[] a1 = [0];
            writeln(i0 is bool == false);
            writeln(i1 is bool == true);
            writeln(s0 is bool == false);
            writeln(s1 is bool == true);
            writeln(a0 is bool == false);
            writeln(a1 is bool == true);
        }
    """))

    assert run_to_flag(emulator, 'win') == b'true\n' * 6

@pytest.mark.parametrize('n, expected', [
    (1, b'false'),
    (2, b'true'),
    (437, b'false'),
    (439, b'true')
])
def test_primality(n, expected):
    # Simpler version of examples/factor.hid
    emulator = make_emulator(compile("""
        bool @prime(int n) {
            if (n < 2) { return false; }
            try {
                int factor = 0;
                int half = n / 2;
                for (int b = 1; b <= half; b *= 2) {
                    preempt { factor += b; }
                    preempt { continue; }
                    break;
                }

                !truth_is_defeat(factor <= 1);
                !truth_is_defeat(n % factor != 0);
                return false;
            } undo {
                return true;
            }
        }

        empty @is_you(int n) {
            write(@prime(n));
        }
    """), [str(n)])

    assert run_to_flag(emulator, 'win') == expected
