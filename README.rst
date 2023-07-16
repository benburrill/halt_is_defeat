==============
Halt is Defeat
==============

Halt is Defeat is an oracle-oriented prophetic (OOP) language targeting
the Sphinx ISA.

Key features of Halt is Defeat:
 * Negative-cost abstractions with time-travel semantics
 * Memory safety through bounds checking and stack-overflow detection
 * Safety from defeat
 * Function overloading
 * Stack-allocated variable-length arrays
 * Type coercion and inference of ambiguous expressions
 * P = NP


Quick start guide
=================
To install the ``hidc`` compiler
[Installation instructions]

The ``hidc`` compiler currently has a single backend, targeting the
Sphinx instruction set architecture.  As of this writing, Sphinx
processors are not yet commercially available, though you may build one
yourself from scratch on a breadboard using flux capacitors and other
off-the-shelf components.

However, the simplest way to run the generated Sphinx assembly code is
under the Sphinx emulator, ``spasm``.  The ``spasm`` emulator is
available for download from https://github.com/benburrill/sphinx

Please note however that the ``spasm`` emulator is written in Python, so
it should not be relied on for performance-critical applications.

Hello world
-----------
The entry point to Halt is Defeat programs is ``@is_you()``.  The return
type must be ``empty``, and parameters specified by ``@is_you`` may be
passed in as command-line arguments to ``spasm`` (more on this later).
There are 3 kinds of function in Halt is Defeat, ``@is_you`` being a
"you" function.  You can tell because the name starts with an ``@``, but
for now you may ignore this distinction.  Here's a simple program:

.. code::

    empty @is_you() {
        writeln("Hello world!");
        write("Some numbers:");
        for (int i = 1; i <= 10; i += 1) {
            write(' ');
            write(i);
        }
        writeln();
    }

Running under ``spasm`` produces the expected output:

.. code::

    $ hidc examples/hello.hid -o hello.s
    $ spasm hello.s
    Hello world!
    Some numbers: 1 2 3 4 5 6 7 8 9 10
    Reached win flag
        CPU time: 633 clock cycles
        Emulator efficiency: 45.61%

If you run it yourself, you may notice something a little odd.  Once the
end of the program is reached, it gets stuck in an infinite loop.

This behavior is necessary in order for Halt is Defeat to ensure that
your programs are undefeatable.  Since "Halt is Defeat", the program
must never halt, so upon returning from ``@is_you()``, Halt is Defeat
code emits the "win" flag (as seen in the output from ``spasm``) and
then loops forever.

If running under the ``spasm`` emulator, you can always issue a keyboard
interrupt (ctrl-c) to forcibly stop the running program.  Attempting to
stop programs running on a real Sphinx processor is not recommended and
may destroy the universe.

When the ``win`` flag is encountered, the ``spasm`` emulator displays
some statistics.  The CPU time is the number of Sphinx instructions
executed (each instruction takes 1 clock cycle).  The emulator
efficiency is an implementation detail of the emulator and may be
ignored.

A taste of defeat
-----------------
Defeat functions have names starting with ``!``, and may result in
defeat if called.  ``!is_defeat()`` always results in defeat.  Defeat
functions cannot be called directly from within you functions, but you
can create a try block within you to safely run code which may lead to
defeat.

In this example we use the ``stop`` handler, which treats defeat in a
similar way to how ``catch`` or ``except`` treat exceptions in other
languages.  When defeat is reached, execution of the ``try`` block stops
and the ``stop`` block is run:

.. code::

    empty @is_you() {
        try {
            writeln("try block");
            !is_defeat();
        } stop {
            writeln("stop block");
        }
    }

Output:

.. code::

    $ hidc stop.hid -o stop.s
    $ spasm stop.s
    try block
    stop block
    Reached win flag
        CPU time: 170 clock cycles
        Emulator efficiency: 36.80%



Note that defeat's utility as a replacement for exceptions is limited.
There is only one form of defeat, and try blocks can ONLY be used within
a "you" context.  Try blocks create a defeat context within the block,
so you cannot nest try blocks as in other languages.

However, the purpose of defeat is not to represent an exceptional
condition -- the purpose of defeat is to be avoided!

Time travel
-----------
Sure, it's nice to know when you've made a mistake.  But more often than
not, we really just wish we could go back and undo it.  In HiD, you can!

The ``undo`` handler works similarly to ``stop``, but it goes back in
time and prevents the ``try`` block from running if it would result in
defeat:

.. code::

    empty @is_you() {
        try {
            writeln("try block");
            !is_defeat();
        } undo {
            writeln("undo block");
        }
    }

Output:

.. code::

    $ hidc undo.hid -o undo.s
    $ spasm undo.s
    undo block
    Reached win flag
        CPU time: 87 clock cycles
        Emulator efficiency: 30.85%


Halting problems
----------------
The ``undo`` block allows us to do some rather interesting things.  If
we modify the above code by putting a loop before the ``!is_defeat()``,
the code will test if the loop will terminate, since defeat would never
occur if the loop runs forever:

.. code::

    empty @is_you() {
        try {
            writeln("The loop runs forever");
            while (true) {}
            !is_defeat();
        } undo {
            writeln("The loop terminates");
        }
    }

Output (it never reaches win because it is stuck in the loop):

.. code::

    $ hidc halting.hid -o halting.s
    $ spasm halting.s
    The loop runs forever


*Hold on a moment... the halting problem of Turing machines is
undecidable, and HiD seems Turing-complete-ish, so what gives?*

For more information on what's really going on here, see
https://github.com/benburrill/sphinx, but to provide some small comfort
that this isn't flagrantly impossible, Sphinx is not Turing complete.
It is "Turing-complete-ish" (similar to how your computer is), but that
only means its halting problem is generally intractable, not undecidable.

Sphinx's entire execution is based around this.  The instruction set
provides only a single jump instruction, the "Turing jump instruction",
which performs a jump if not jumping would lead to halting.


Computational astrology
-----------------------
While the try/undo construct can be useful on its own, the most powerful
and flexible tool in your temporal arsenal is ``preempt``.  It works a
bit like an inside-out try.  The ``preempt`` block is run if not running
the ``preempt`` block would lead to defeat.  It is used within ``try``
blocks.


Other features
==============

Arrays and strings
------------------

HiD does not have heap-based dynamic allocation, so in order to enable
safe stack-based array allocation, HiD imposes a few restrictions on the
use of arrays:

- Arrays cannot be returned from functions.  If you want to "return" an
  array, you must take an output parameter and mutate it.
- Array variables (ie the reference to the array) cannot be reassigned.
  In essence, the constness of an array applies only to its *elements*,
  not the reference itself (which is always const).

There are two ways to create a new array.  Array literals, such as
``[1, 2, f(x)]`` are free expressions, which may be assigned to an array
variable ``baba`` like ``int[] baba = [1, 2, f(x)];`` (or
``const int[] baba = [1, 2, f(x)];``).
Array initializers are a special syntax for creating uninitialized array
variables of arbitrary length: ``int baba[f(x)];``

When passed to functions, a non-const array may be coerced into a const
array, but not vice-versa.  This means that unless you intend on
mutating the arrays passed to your functions, you should write your
functions to take const arrays.

Boolean arrays are bit-vectors, so they are memory-efficient, but a bit
slow.

Strings are similar to ``const byte[]``, and in fact may be implicitly
coerced to ``const byte[]``.  However, strings make a different set of
tradeoffs than arrays.  Strings cannot be dynamically created, but they
can be used as freely as other scalar types -- they can be returned,
reassigned, and used as elements of an array (whereas arrays cannot be
nested).

The length of the array or string ``baba`` may be obtained with
``baba.length``.

The speculation operator
------------------------
The speculation operator, ``??`` may be used as an alternative to using
``try/undo`` for the simple case where you simply want to avoid
evaluating an expression unless it would produce an "unexpected" value.
For example ``sum(arr) ?? 0`` will not evaluate ``sum(arr)`` and simply
return 0 if evaluating ``sum(arr)`` would have returned 0.  Otherwise,
``sum(arr)`` will be evaluated and the result returned.

The right-hand side will always be evaluated, and if both sides are
evaluated, the right side will be evaluated first.

Just like ``try``, speculation can only be used by you.
Additionally, the operands of ``??`` must be ordinary expressions --
neither you-functions nor defeat-functions may be used.

Command-line arguments
----------------------
Halt is Defeat makes use of Sphinx's robust argument specifiers, which I
added to Sphinx mostly so that Halt is Defeat could make use of them.

If you want command-line arguments, you can write your ``@is_you``
function with the signature ``empty @is_you(const string[] args)``

Does your program take integers as input?  Don't want to write code to
parse them?  Don't worry!  You can get ``spasm`` to do it for you!
The signature ``empty @is_you(const int[] args)`` specifies that the
inputs should be integers, which ``spasm`` will be parse (in base 10)
from the command line arguments.

You can even mix and match:
``empty @is_you(string mode, const int[] args)``

In addition to convenience, an advantage to this is that the cycle count
reported by ``spasm`` won't get artificially inflated by parsing code,
which is useful in evaluating the performance of your time-traveling
algorithms.

Caveats:

- You may only have at most one array in the parameters of ``@is_you``.
  If you want anything more complicated you'll need to take an array of
  strings and do the parsing yourself.
- Neither bool nor bool[] are not allowed as parameters to ``@is_you``
- Although int[] and byte[] may be either const or non-const, string[]
  passed to ``@is_you()`` must be const.  If you want a mutable array of
  string arguments, you'll need to copy them over:

.. code::

    empty @is_you(const string[] args) {
        string mutargs[args.length];
        for (int i = 0; i < args.length; i += 1) {
            mutargs[i] = args[i];
        }
    }

Increasing the word size and stack size
---------------------------------------

By default, ``hidc`` targets a 16-bit word size, and provides 500 words
of stack space.  These can both be increased.

- To change the word size, use ``-m``, eg ``-m24`` to target 24 bits.
- To change the stack size, use ``-s``, eg ``-s1000`` for 1000 words.

As an alternative to increasing the stack size, you may also consider
making your variables/arrays global, and where possible making them
const.

Although increasing the word and stack size can increase the size of the
problems you can solve with HiD, be wary of the exponential tendencies
of emulation under ``spasm`` -- you may want to take things slow.
There's no prize for writing a program that requires more RAM in order
to emulate than could fit in the observable universe, it just means you
need a better computer.

If using 24 bit words, I'd recommend decreasing the stack size to 100
words or less for testing, so that if you accidentally write a tight
24-bit loop ``spasm`` won't need more than 8 GB if that loop needs to be
predicted (assuming no non-const globals).  If using more than 24 bits,
you are on your own, god help you.

Fatal errors and undefined behavior
-----------------------------------

Fatal errors occur when invalid operations are performed, such as
dividing by 0.  Errors are different from defeat, and in fact provide
safety from defeat similar to the ``win`` state.  As a result of this,
the path of execution leading up to an error might not have actually
occurred if the conditions that produced the error were fixed.

For example, in a try/undo block you might have a path of execution
which "should" lead to defeat, but instead causes a stack overflow.
This could cause code to run which otherwise wouldn't if the stack size
were increased, possibly printing "incorrect" output leading up to the
error.  This can be confusing, but it is much more useful in debugging
the causes of an error than if such errors caused defeat.

Errors can also be produced in user code with ``all_is_broken()``.

Although many operations in HiD are checked and will produce errors, the
following are undefined behavior:

- Accessing uninitialized strings in dynamically allocated arrays
- Dynamically allocating an array with negative length (usually this
  will produce a stack-overflow error, but not necessarily)

Additionally, if the ``--unchecked`` flag is passed, all previously
checked operations become undefined behavior:

- Division or modulo by 0
- Indexing an array or string out of bounds
- Stack overflow

Be aware that HiD's nasal demons can time travel, so undefined behavior
may result in a program's defeat before it even starts, etc.

Language reference
==================

Standard library
----------------
- ``empty write(string s)`` / ``empty write(const byte[] s)`` - writes
  out all bytes of data from the string / byte array
- ``empty write(int i)`` - writes the integer in base 10
- ``empty write(byte b)`` - writes a single byte of data
- ``empty write(bool b)`` - writes the string "true" or "false"
- ``empty writeln()`` - equivalent to ``write('\n')``
- ``empty writeln(T x)`` - ``write(x)`` followed by ``writeln()``
- ``empty !is_defeat()`` - causes defeat
- ``empty !truth_is_defeat(bool cond)`` - equivalent to ``if (cond) { !is_defeat(); }``
- ``empty all_is_win()`` - enter the win state, ending program execution and starting infinite loop
- ``empty all_is_broken()`` - enter the error state, ending program execution and starting infinite loop
- ``empty sleep(int millis)`` - sleep for the given number of milliseconds
- ``empty debug()`` - emits debug flag, results are implementation dependent.  On ``spasm`` will dump memory to stdout.

Operators
---------
In order of precedence:
 * Unary ``+``, ``-``, ``not``
 * ``is`` (typecast pseudo-operator)
 * ``*``, ``/``, ``%``
 * ``+``, ``-``
 * ``==``, ``!=``, ``<``, ``<=``, ``>``, ``>=``
 * ``and`` (short-circuiting logical and)
 * ``or`` (short-circuiting logical or)
 * ``??`` (speculation)

Augmented assignments: ``+=``, ``-=``, ``*=``, ``/=``, ``%=``

Types
-----
The ``empty`` type signifies a lack of value, it may only be used as a
return type.

Array types, eg ``int[]`` refer to sequences of scalar values.

Scalar types:
 * ``int`` - Word-sized numeric type
 * ``byte`` - Byte-sized data type.  Coercible to ``int``.
 * ``bool`` - Boolean value, either ``true`` or ``false``.
 * ``string`` - Nominally utf-8 encoded byte-string.  Coercible to ``const byte[]``.

For scalar types, constness is an attribute of variables, not of their
type, and determines whether the variable can be reassigned.  For array
types, constness is part of the type, and the variable referring to the
array can never be reassigned.

Some literals have special coercion rules.

Literals:
 * Numeric literals: ``5``, ``0xFF``, ``1_000``, etc - Type: ``int``, but coercible to ``byte``
 * Character literals: ``'a'``, ``'\n'`` - Type: ``byte``
 * String literals: "Hello \u{1F30E}" - Type: ``string``
 * Boolean literals: ``true`` or ``false`` - Type: ``bool``
 * Array literals: ``[1, 2, f(x)]`` - Type: array of the first type all
   entries can be coerced to, but is coercible to any type all entries
   can be coerced to.  Preferentially ``const``, but may be coerced to
   non-const.

Explicit type casts may be performed with ``is``, eg ``baba is byte``.

Allowed explicit type casts:
 * (``byte`` | ``bool``) ``is int``
 * (``int`` | ``bool``) ``is byte``
 * (``int`` | ``byte`` | ``string`` | ``T[]``) ``is bool`` (arrays and
   strings are considered ``true`` if they have non-zero length)
 * (``string``) ``is byte[]`` (result is ``const byte[]``)
 * (array literal) ``is T[]`` (valid if all entries in the array literal
   can be cast to ``T``, and retains the ``const`` flexibility of array
   literals).
