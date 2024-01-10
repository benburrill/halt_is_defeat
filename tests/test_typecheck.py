from hidc.lexer import SourceCode
from hidc.parser import parse
from hidc.ast import Environment
from hidc.errors import TypeCheckError

from pytest import raises


def assert_good(string, **options):
    unchecked = parse(SourceCode.from_string(string))
    checked = unchecked.evaluate(Environment.empty(**options))
    # These are dummy assertions which are kinda pointless to check
    # Ideally I'd like to walk through the tree and make sure all types
    # match up, as they should after successful typechecking
    assert len(unchecked.var_decls) == len(checked.var_decls)
    assert len(unchecked.func_decls) == len(checked.func_decls)

def assert_bad(string, **options):
    unchecked = parse(SourceCode.from_string(string))
    with raises(TypeCheckError):
        unchecked.evaluate(Environment.empty(**options))


def test_return_type():
    assert_good("""
        int f() {
            return 10;
        }
    """)

    assert_bad("""
        int f() {
            return "10";
        }
    """)

    assert_good("""
        int f(int x) {
            return x;
        }
    """)

    assert_good("""
        int f(byte x) {
            return x;  // Implicit coercion
        }
    """)

    assert_bad("""
        byte f(int x) {
            return x;
        }
    """)


def test_exit():
    assert_good("""
        int f(int x) {
            if (x < 10) {
                return 42;
            } else {
                return 10;
            }
        }
    """)

    assert_bad("""
        int f(int x) {
            if (x < 10) {
                return 42;
            }
        }
    """)

    assert_good("""
        empty f(int x) {
            if (x < 10) {
                return;
            }
        }
    """)


def test_const_arrays():
    assert_good("""
        empty f(const int[] x) {}
        empty g(int[] x) {
            f(x);
        }
    """)

    assert_bad("""
        empty f(int[] x) {}
        empty g(const int[] x) {
            f(x);
        }
    """)

    assert_good("""
        empty f() {
            const int[] x = [1, 2, 3];
            const int[] y = x;
        }
    """)

    assert_bad("""
        empty f() {
            const int[] x = [1, 2, 3];
            int[] y = x;
        }
    """)

    assert_bad("""
        empty f() {
            int[] x = [1, 2, 3];
            const int[] y = x;
        }
    """)

    assert_good("""
        empty f() {
            int x[10];
        }
    """)

    assert_bad("""
        empty f() {
            const int x[10];
        }
    """)

def test_coercion():
    assert_good("""
        empty f() {
            int x = 'a';
            byte y = 10;
            byte z = 1 + 1;
            byte w = -1;
        }
    """)

    assert_bad("""
        empty f() {
            int x = true;
            bool y = 10;
        }
    """)

    assert_bad("""
        empty f() {
            const int x = 10;
            byte y = x;
        }
    """)

    assert_good("""
        empty f() {
            int[] x = [1, 'a'];
            byte[] y = [1, 'a'];
        }
    """)

    assert_good("""
        empty f() {
            byte x = 1;
            int[] y = [x, 1];
        }
    """)

    assert_bad("""
        empty f() {
            int x = 1;
            byte[] y = [x, 'a'];
        }
    """)

    assert_good("""
        empty f() {
            const byte x = 10;
            byte y = x;
            y = y + 1;
            byte z = x + 1;
        }
    """)

    assert_good("""
        empty f() {
            byte b = 1;
            byte x = 0;
            x += b;
        }
    """)

    assert_bad("""
        empty f() {
            int i = 1;
            byte x = 0;
            x += i;
        }
    """)

    assert_good("""
        empty f() {
            byte x = 0;
            x += 1;
        }
    """)

    assert_bad("""
        empty f() {
            byte x = 0;
            x += 1 is int;
        }
    """)

    assert_good("""
        empty f() {
            byte x = 0;
            x += 1 is byte;
        }
    """)

    assert_good("""
        empty f() {
            byte x = 0;
            x += (1 is int) is byte;
        }
    """)

    assert_good("""
        empty f(byte x) {
            byte y = x * 3 + 4;
            byte z = x * 3 + y * 5;
        }
    """)

    assert_good("""
        empty f(byte b, int i) {
            int x = b * 2 + i * 3;
        }
    """)

    assert_bad("""
        empty f(byte b, int i) {
            byte x = b * 2 + i * 3;
        }
    """)


def test_cast():
    assert_good("""
        empty f() {
            int x = true is int;
            bool y = 10 is bool;
        }
    """)


def test_bad_array():
    assert_good("""
        empty f() {
            [1, 2];
        }
    """)

    assert_bad("""
        empty f() {
            [[1], [2]];
        }
    """)


def test_unreachable():
    assert_good("""
        empty f() {
            return;
            write("hi");
        }
    """)

    assert_bad("""
        empty f() {
            return;
            write("hi");
        }
    """, unreachable_error=True)

    assert_good("""
        empty f() {
            write("hi");
            return;
        }
    """, unreachable_error=True)


    assert_good("""
        empty @f(int x) {
            while (true) {
                try {
                    if (x < 10) {
                        break;
                    } else {
                        !is_defeat();
                    }
                } undo {
                    break;
                }
            }
        }
    """, unreachable_error=True)

    assert_bad("""
        empty @f(int x) {
            while (true) {
                try {
                    if (x < 10) {
                        break;
                    } else {
                        !is_defeat();
                    }
                } undo {
                    break;
                }
                writeln("Hello world!");
            }
        }
    """, unreachable_error=True)

    assert_good("""
        empty @f(int x) {
            while (true) {
                try {
                    if (x < 10) {
                        writeln("Hi");
                    } else {
                        !is_defeat();
                    }
                } undo {
                    break;
                }
                writeln("Hello world!");
            }
        }
    """, unreachable_error=True)

def test_operators():
    assert_good("""
        empty f(int x, byte y) {
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
    assert_good("""
        int x = 3;
        int y = 10;
        empty f(int x) {
            int y = 8;
        }
    """)

    assert_bad("""
        empty f() {
            int x = 3;
            int x = 8;
        }
    """)

    assert_bad("""
        empty f() {
            int x = 3;
            if (true) {
                int x = 8;
            }
        }
    """)

    assert_bad("""
        empty f(int x) {
            int x = 3;
        }
    """)

    assert_bad("""
        empty f(int x, int x) {}
    """)

def test_loop():
    assert_good("""
        empty f() {
            for (int i = 0; i < 10; i += 1) {
                write(i);
            }
            
            int i = 42; // Different scope
        }
    """)

    assert_good("""
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

    assert_bad("""
        int g() {
            for (;;) {
                break;
            }
        }
    """)

    assert_good("""
        int f() {
            while (true) {
                return 1;
            }
        }
    """)

    assert_bad("""
        int f(int x) {
            while (true) {
                if (x < 10) {
                    break;
                }
                
                return 1;
            }
        }
    """)

    assert_good("""
        int f(int x) {
            while (true) {
                if (x < 10) {
                    break;
                }
                
                return 1;
            }
            return 0;
        }
    """)

def test_try():
    assert_good("""
        int @f() {
            try {
                !is_defeat();
            } undo {
                return 5;
            }
        }
    """)

    assert_good("""
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

    assert_bad("""
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
    assert_good("""
        empty f() {
            int x[10];
            x.length;
            int l = x.length;
            x[0];
            int x0 = x[0];
        }
    """)

    assert_bad("""
        empty f() {
            int x = 0;
            x.length;
        }
    """)

    assert_bad("""
        empty f() {
            int x = 0;
            x[0];
        }
    """)

    assert_bad("""
        empty f() {
            int x[10];
            x["hi"];
        }
    """)

    assert_good("""
        empty f() {
            string x = "hello";
            int l = x.length;
            byte x0 = x[0];
        }
    """)


def test_func_calls():
    assert_good("""
        empty f(const int[] x) {}
        empty g() {
            f([]);
        }
    """)

    # Ambiguous call uses first matching signature
    assert_good("""
        int f(const int[] x) {return 0;}
        string f(const string[] x) {return "";}
        empty g() {
            int x = f([]);
        }
    """)

    assert_bad("""
        string f(const string[] x) {return "";}
        int f(const int[] x) {return 0;}
        empty g() {
            int x = f([]);
        }
    """)

    # If there's an exact match, it is selected
    assert_good("""
        int f(const int[] x) {return 0;}
        byte f(const byte[] x) {return 0;}
        empty g() {
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
    assert_bad("""
        int f(int[] x) {return 0;}
        byte f(byte[] x) {return 0;}
        empty g() {
            byte x = f(['a']);
        }
    """)

    assert_good("""
        int f(int[] x) {return 0;}
        byte f(byte[] x) {return 0;}
        empty g() {
            int x = f(['a']);
        }
    """)

    # However, this ambiguity can be resolved with an explicit type cast
    assert_good("""
        int f(int[] x) {return 0;}
        byte f(byte[] x) {return 0;}
        empty g() {
            byte x = f(['a'] is byte[]);
        }
    """)


def test_undeclared():
    assert_bad("""
        empty f() {
            write(x);
        }
    """)

    assert_bad("""
        empty f() {
            g();
        }
    """)

def test_redecl():
    assert_bad("""
        int x = 10;
        int x = 5;
    """)

    assert_bad("""
        empty f() {
            int x = 10;
            int x = 5;
        }
    """)

    assert_good("""
        int x = 10;
        empty f() {
            // Shadowing of globals allowed
            int x = 5;
        }
    """)

    assert_bad("""
        empty f() {
            int x = 10;
            if (true) {
                // Shadowing of locals not allowed
                int x = 5;
            }
        }
    """)

    assert_good("""
        empty f(int x) {}
        empty f(byte x) {}
    """)

    assert_bad("""
        empty f(int x) {}
        empty f(int x) {}
    """)

    assert_bad("""
        empty writeln(int x) {}
    """)
