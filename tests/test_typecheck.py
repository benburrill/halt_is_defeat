from hidc.parser import parse
from hidc.errors import TypeCheckError
from hidc.lexer import SourceCode

from pytest import raises


def assert_valid(string):
    unchecked = parse(SourceCode.from_string(string))
    checked = unchecked.checked()
    # These are dummy assertions which are kinda pointless to check
    # Ideally I'd like to walk through the tree and make sure all types
    # match up, as they should after successful typechecking
    assert len(unchecked.var_decls) == len(checked.var_decls)
    assert len(unchecked.func_decls) == len(checked.func_decls)

def assert_invalid(string):
    unchecked = parse(SourceCode.from_string(string))
    with raises(TypeCheckError):
        unchecked.checked()


def test_return_type():
    assert_valid("""
        int f() {
            return 10;
        }
    """)

    assert_invalid("""
        int f() {
            return "10";
        }
    """)

    assert_valid("""
        int f(int x) {
            return x;
        }
    """)

    assert_valid("""
        int f(byte x) {
            return x;  // Implicit coercion
        }
    """)

    assert_invalid("""
        byte f(int x) {
            return x;
        }
    """)


def test_exit():
    assert_valid("""
        int f(int x) {
            if (x < 10) {
                return 42;
            } else {
                return 10;
            }
        }
    """)

    assert_invalid("""
        int f(int x) {
            if (x < 10) {
                return 42;
            }
        }
    """)

    assert_valid("""
        void f(int x) {
            if (x < 10) {
                return;
            }
        }
    """)


def test_const_arrays():
    assert_valid("""
        void f(const int[] x) {}
        void g(int[] x) {
            f(x);
        }
    """)

    assert_invalid("""
        void f(int[] x) {}
        void g(const int[] x) {
            f(x);
        }
    """)

    assert_valid("""
        void f() {
            const int[] x = [1, 2, 3];
            const int[] y = x;
        }
    """)

    assert_invalid("""
        void f() {
            const int[] x = [1, 2, 3];
            int[] y = x;
        }
    """)

    assert_invalid("""
        void f() {
            int[] x = [1, 2, 3];
            const int[] y = x;
        }
    """)

    assert_valid("""
        void f() {
            int x[10];
        }
    """)

    assert_invalid("""
        void f() {
            const int x[10];
        }
    """)

def test_coercion():
    assert_valid("""
        void f() {
            int x = 'a';
            byte y = 10;
        }
    """)

    assert_invalid("""
        void f() {
            int x = true;
            bool y = 10;
        }
    """)

    assert_invalid("""
        void f() {
            int x = 10;
            byte y = x;
        }
    """)

    assert_valid("""
        void f() {
            int[] x = [1, 'a'];
            byte[] x = [1, 'a'];
        }
    """)

    assert_valid("""
        void f() {
            byte x = 1;
            int[] x = [x, 1];
        }
    """)

    assert_invalid("""
        void f() {
            int x = 1;
            byte[] x = [x, 'a'];
        }
    """)

    assert_valid("""
        void f() {
            byte x = 0;
            x += 1;
        }
    """)

    assert_valid("""
        void f() {
            byte x = 0;
            byte y = x * 3 + 2;
        }
    """)


def test_cast():
    assert_valid("""
        void f() {
            int x = true is int;
            bool y = 10 is bool;
        }
    """)


def test_bad_array():
    assert_valid("""
        void f() {
            [1, 2];
        }
    """)

    assert_invalid("""
        void f() {
            [[1], [2]];
        }
    """)


def test_unreachable():
    assert_valid("""
        void f() {
            print("hi");
            return;
        }
    """)

    assert_invalid("""
        void f() {
            return;
            print("hi");
        }
    """)
