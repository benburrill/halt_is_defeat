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
            byte[] y = [1, 'a'];
        }
    """)

    assert_valid("""
        void f() {
            byte x = 1;
            int[] y = [x, 1];
        }
    """)

    assert_invalid("""
        void f() {
            int x = 1;
            byte[] y = [x, 'a'];
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

def test_operators():
    assert_valid("""
        void f(int x, byte y) {
            int a = 2 + 3;
            int b = 2 + y;
            int c = x + y;
            bool d = 1 < 2;
            bool e = x < y;
            bool f = d and e;
            bool g = not f;
            bool h = x == y;
            bool i = g == h;
            int j = -x;
            int k = -y;
            byte l = -y;
        }
    """)

def test_shadowing():
    assert_valid("""
        int x = 3;
        int y = 10;
        void f(int x) {
            int y = 8;
        }
    """)

    assert_invalid("""
        void f() {
            int x = 3;
            int x = 8;
        }
    """)

    assert_invalid("""
        void f() {
            int x = 3;
            if (true) {
                int x = 8;
            }
        }
    """)

    assert_invalid("""
        void f(int x) {
            int x = 3;
        }
    """)

    assert_invalid("""
        void f(int x, int x) {}
    """)

def test_loop():
    assert_valid("""
        void f() {
            for (int i = 0; i < 10; i += 1) {
                print(i);
            }
            
            int i = 42; // Different scope
        }
    """)

    assert_valid("""
        int f(int x) {
            if (x < 10) {
                while (true) {}
            } else {
                return 5;
            }
        }
        
        int g() {
            for (;;) {}
        }
    """)

    assert_invalid("""
        int g() {
            for (;;) {
                break;
            }
        }
    """)

def test_try():
    assert_valid("""
        int @f() {
            try {
                !is_defeat();
            } undo {
                return 5;
            }
        }
    """)

    assert_valid("""
        int @f() {
            try {
                preempt {
                    !is_defeat();
                }
                !is_defeat();
            } undo {
                return 5;
            }
        }
    """)

    assert_invalid("""
        int @f() {
            try {
                preempt {
                    !is_defeat();
                }
            } undo {
                return 5;
            }
        }
    """)

def test_lookups():
    assert_valid("""
        void f() {
            int x[10];
            x.length;
            int l = x.length;
            x[0];
            int x0 = x[0];
        }
    """)

    assert_invalid("""
        void f() {
            int x = 0;
            x.length;
        }
    """)

    assert_invalid("""
        void f() {
            int x = 0;
            x[0];
        }
    """)

    assert_invalid("""
        void f() {
            int x[10];
            x["hi"];
        }
    """)

    assert_valid("""
        void f() {
            string x = "hello";
            int l = x.length;
            byte x0 = x[0];
        }
    """)


def test_func_calls():
    assert_valid("""
        void f(const int[] x) {}
        void g() {
            f([]);
        }
    """)

    # Ambiguous call uses first matching signature
    assert_valid("""
        int f(const int[] x) {return 0;}
        string f(const string[] x) {return "";}
        void g() {
            int x = f([]);
        }
    """)

    assert_invalid("""
        string f(const string[] x) {return "";}
        int f(const int[] x) {return 0;}
        void g() {
            int x = f([]);
        }
    """)

    # If there's an exact match, it is selected
    assert_valid("""
        int f(const int[] x) {return 0;}
        byte f(const byte[] x) {return 0;}
        void g() {
            byte x = f(['a']);
        }
    """)

    # However, ArrayLiterals are preferentially const, so there is no
    # exact match, and it instead coerces to f(int[])
    #
    # This is a quirk that I'm not sure is desired behavior.
    # constness is an important part of signature, and you normally
    # cannot coerce const arrays to non-const, nor can byte[] even be
    # coerced to int[].  ArrayLiterals are special in that they allow
    # both coercions, but ideally would "prefer" const coercions over
    # coercion of preferred ArrayLiteral element type when matching
    # signatures.
    assert_invalid("""
        int f(int[] x) {return 0;}
        byte f(byte[] x) {return 0;}
        void g() {
            byte x = f(['a']);
        }
    """)

    assert_valid("""
        int f(int[] x) {return 0;}
        byte f(byte[] x) {return 0;}
        void g() {
            int x = f(['a']);
        }
    """)

    # However, this ambiguity can be resolved with an explicit type cast
    assert_valid("""
        int f(int[] x) {return 0;}
        byte f(byte[] x) {return 0;}
        void g() {
            byte x = f(['a'] is byte[]);
        }
    """)


def test_undeclared():
    assert_invalid("""
        void f() {
            print(x);
        }
    """)

    assert_invalid("""
        void f() {
            g();
        }
    """)
