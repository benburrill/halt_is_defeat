==============
Halt is Defeat
==============

Halt is Defeat is an oracle-oriented prophetic (OOP) language targeting
the Sphinx ISA.

Key features of Halt is Defeat:
 * Negative-cost abstractions with time-travel semantics
 * Guaranteed memory safety / bounds checking and stack-overflow detection
 * Guaranteed safety from defeat
 * Function overloading
 * Stack-allocated variable-length arrays
 * Type coercion and inference of ambiguous expressions
 * Solve any NP problem in polynomial time

Quick start guide
=================
The ``hidc`` compiler requires Python >= 3.10.  It has no other
dependencies and may be run directly from this directory:
``python3 -m hidc examples/hello.hid -o hello.s``

Alternatively, you may install the ``hidc`` executable:

.. code::

    $ pip3 install --editable .
    $ hidc examples/hello.hid -o hello.s

The ``hidc`` compiler has a single backend, targeting the Sphinx
instruction set architecture.  As of this writing, Sphinx processors are
not yet commercially available due to global electronics shortages of
some critical components, such as flux capacitors.

The simplest way to run the generated Sphinx assembly code is
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
        CPU time: 578 clock cycles
        Emulator efficiency: 48.78%

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
            writeln("> try block");
            !is_defeat();
        } stop {
            writeln("> stop block");
        }
    }

Output:

.. code::

    $ hidc stop.hid -o stop.s
    $ spasm stop.s
    > try block
    > stop block
    Reached win flag
        CPU time: 175 clock cycles
        Emulator efficiency: 43.32%

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
            writeln("> try block");
            !is_defeat();
        } undo {
            writeln("> undo block");
        }
    }

Output:

.. code::

    $ hidc undo.hid -o undo.s
    $ spasm undo.s
    > undo block
    Reached win flag
        CPU time: 90 clock cycles
        Emulator efficiency: 42.06%

The try block is skipped entirely, saving us 85 clock cycles!

Halting problems
----------------
One nice thing about try/undo is that like an if/else, try and undo are
mutually exclusive.  Either the try block or the undo block will run,
but never both.

If we modify the previous code by putting a loop before ``!is_defeat()``,
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

If we ignore the question of *how* the try block can know "in advance"
whether or not to run, there is nothing problematic about *what* it lets
us do.  Even though there is a difference from the perspective of the
user, try/undo does not really let us test in isolation if some code is
non-terminating any more so than try/stop does.

As for *how* it works, to provide some comfort that it isn't flagrantly
impossible, Sphinx (HiD's compilation target) is **not** strictly Turing
complete (no computer with finite memory is TC), so Sphinx's halting
problem need not be undecidable.  The Sphinx emulator is not solving the
halting problem of Turing machines, only of Sphinx.

For more information on what's really going on here, see
https://github.com/benburrill/sphinx.

Sphinx's entire execution is dependent on its own halting problem.  The
instruction set provides only a single jump instruction, the "Turing
jump instruction", which performs a jump if not jumping would lead to
halting.

Another fun implementation detail is that because Sphinx has no other
jump instructions, even ``if`` statements in Halt is Defeat must predict
the future, and work by propagating future halts backwards through time.
We've been solving Sphinx's halting problem since Hello World!

Computational astrology
-----------------------
While the try/undo construct can be useful on its own, the most powerful
and flexible tool in your temporal arsenal is ``preempt``.

The ``preempt`` block is run if not running the ``preempt`` block would
lead to defeat.  It is used within ``try`` blocks.

Here we use the ``preempt`` block to write a function that finds the
maximum value of an array, returning early as soon as this value is
first encountered:

.. code::

    int @max(const int[] arr) {
        try {
            int max_val = arr[0];
            for (int i = 1; i < arr.length; i += 1) {
                if (arr[i] > max_val) {
                    max_val = arr[i];
                    preempt {
                        return max_val;
                    }
                }
            }

            !is_defeat();
        } undo {
            return arr[0];
        }
    }

We have placed ``!is_defeat()`` at the end of the loop, which means that
it would be defeat for the loop to complete normally.  The ``return``
provides the only escape from this looming existential threat, but the
``if`` statement means we only have the opportunity to take the return
whenever we find a value larger than any previous one.

The ``preempt`` block containing the return will run if not doing so
would lead to defeat.  If no larger value will be found, the ``preempt``
block needs to be run, since otherwise we'd be heading to defeat at the
end of the loop.  However, if there's a larger value in the future, the
``preempt`` block is not run -- a return will need to occur in the
future, so we are safe from defeat in the present.

In this code, ``preempt`` is doing most of the work.  The ``try/undo``
is mostly serving just to set up an arena for ``preempt`` to be used,
and only handles the special case where ``arr[0]`` is the maximum.

If you want to test it out, the full program can be found in
`<examples/max.hid>`_, and takes command-line arguments so you can
easily play around with different inputs.

*What time-traveling algorithms can YOU come up with?*

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
variables of arbitrary length: ``int baba[keke];``

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

Preemptive defeat functions
---------------------------
You can use ``preempt`` in defeat functions, but with some restrictions.

The ``preempt`` block is inherently nonlocal, but usually we want at
least *some* locality when using it.  When ``preempt`` is used directly
within a try block, the locality is provided by the try block.  However,
when used within a defeat function, the intuitive behavior is for it to
be local to the function's scope rather than to the parent try block, as
that would break function modularity.

Since Sphinx provides us with only a single "channel" of defeat, we
cannot distinguish between defeat that occurs locally within the
function we are in and defeat that is caused by our caller (which could
also be a defeat function).  For this reason, Halt is Defeat was
originally designed with the idea that preempt would never be allowed
inside of defeat functions.

However, having preempt in defeat functions is very useful for writing
recursive time-traveling algorithms
(such as `<examples/mergesort.hid>`_).
These sorts of functions require their caller to guarantee safety from
defeat in order to work correctly, but they pass along this safety to
their recursive calls.

The problem still remains that we can't really guarantee at compile time
that preemptive defeat functions are being provided appropriate safety.
Halt is Defeat conservatively requires that all defeat functions with a
preempt block in them (even if the preempt block is totally unreachable)
must always be guaranteed safety.  Unless disabled with ``--unchecked``,
Halt is Defeat will check at runtime to ensure that safety is provided
to such functions at the return boundary, producing a fatal runtime
error if it is not.

This means that the following code will produce an error:

.. code::

    empty !baba() {
        if (false) { preempt {} }
    }

    empty @is_you() {
        try {
            !baba();
            !is_defeat();
        } undo {}
    }

However, if either the defeat or the preempt is removed, or if the
function is inlined, or if the code is compiled with ``--unchecked``,
there will be no error.

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
to emulate than could physically fit in the observable universe, it just
means you need a better computer.

Fatal errors and undefined behavior
-----------------------------------

Fatal errors occur when invalid operations are performed, such as
dividing by 0.  Errors are different from defeat, and in fact provide
safety from defeat similar to the ``win`` state.  As a result of this,
the path of execution leading up to an error might not have actually
occurred if the conditions that produced the error were fixed.

For example, a try/undo block might have a stack overflow error which
prevents the code from continuing on to reach a later defeat.  If the
stack size were increased, the try block would have lead to defeat, and
so the try block "shouldn't" have been run to begin with.  This could
cause the printed output leading up to the error to seem "incorrect" or
misleading if it is outputted under the assumption that defeat will be
reached in the future.  This is actually very useful behavior for
debugging, as it shows you under what condition the error actually
occurs, but you need to be mindful of it.

Errors can also be produced in user code with ``all_is_broken()``.

Although many operations in HiD are checked and will produce runtime
errors, there is some undefined behavior in the case of dynamically
allocated arrays.  Dynamically allocated arrays are uninitialized, and
the use of uninitialized strings is undefined behavior.

Additionally, if the ``--unchecked`` flag is passed, all previously
checked operations become undefined behavior:

- Division or modulo by 0
- Indexing an array or string out of bounds
- Stack overflow
- Dynamically allocating an array with negative length
- Calling a preemptive defeat function without providing safety

Be aware that HiD's nasal demons can time travel, so undefined behavior
can produce effects which are difficult to debug, such as killing the
programmer's grandfather before they were born.

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
- ``empty debug()`` - emits debug flag, results are platform-dependent.  ``spasm`` will dump memory to stdout.
- ``empty progress()`` - emits progress flag.  ``spasm`` will display elapsed time between progress flags.

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

Array types, eg ``int[]`` are sequences of scalar values.

Scalar types:
 * ``int`` - Word-sized signed numeric type.
 * ``byte`` - Byte-sized data type.  Coercible to ``int``.
 * ``bool`` - Boolean value, either ``true`` or ``false``.
 * ``string`` - Nominally utf-8 encoded byte-string.  Coercible to ``const byte[]``.

For scalar types, constness is an attribute of variables, not of their
type, and determines whether the variable can be reassigned.  For array
types, constness *is* part of the type, and determines whether array
elements can be modified.  The variable referring to an array can never
be reassigned.

Types and special coercion rules of literals:
 * Numeric literals: ``5``, ``0xFF``, ``1_000`` - Type: ``int``, but coercible to ``byte``
 * Byte/"character" literals: ``'a'``, ``'\n'`` - Type: ``byte``
 * String literals: ``"Hello \u{1F30E}"`` - Type: ``string``
 * Boolean literals: ``true`` or ``false`` - Type: ``bool``
 * Array literals: ``[1, 2, f(x)]`` - Type: array of the first type all
   entries can be coerced to, but is coercible to any type all entries
   can be coerced to.  Preferentially ``const``, but may be coerced to
   non-const.

Arithmetic operators (``+``, ``-``, ``*``, ``/``, ``%``) take operands
of either ``byte`` or ``int`` and always produce values of type ``int``.
However, if all of the operands are coercible to ``byte``, the resulting
value is also coercible to ``byte``.

Explicit type casts may be performed with ``is``, eg ``baba is byte``.

Allowed explicit type casts:
 * (byte | bool) ``is int``
 * (int | bool) ``is byte``
 * (int | byte | string | array) ``is bool`` - strings and arrays are
   truthy if they have non-zero length
 * (string) ``is byte[]`` - result is ``const byte[]``
 * (array literal) ``is T[]`` - valid if all entries in the array
   literal can be cast to ``T``, and retains the ``const`` flexibility
   of array literals.

Blocks and functions
--------------------
Control blocks can be followed by any other block, but not by line
statements.  So ``preempt if (baba) { keke(); }`` is valid, but not
``preempt keke();``.

Try blocks
..........
``try`` blocks must have either a ``stop`` or ``undo`` handler.  The
``stop`` handler is run if defeat is reached in the try block (similar
to catch or except in other languages).  The ``undo`` block is run
instead of the ``try`` block if running the try block *would have* lead
to defeat.  ``try`` can only be used in a "you" context, and so cannot
be nested.

Regardless of handler, ``try`` blocks can contain any number of
``preempt`` blocks.  The ``preempt`` block is run if not running the
``preempt`` block would lead to defeat in its parent ``try`` block.

Functions
.........
You functions (prefixed by ``@``) have special calling restrictions to
ensure a return path that will never reach defeat.  This invariant is
what allows ``try`` blocks and speculation to be used in a modular and
consistent way, so these may only be used within you-functions.
To maintain this, you-functions can *only* be called directly from
within other you-functions (and not within ``try`` blocks since those
represent a code path that could lead to defeat).

Defeat functions (prefixed by ``!``) can cause defeat.  They may call
other defeat functions, just as you can in a ``try`` block.  Defeat
functions can also have ``preempt`` blocks.  A defeat function which
contains a preempt block anywhere in it (even if unreachable) is called
a "preemptive defeat function".  The caller of a preemptive defeat
function must guarantee safety (for example, by wrapping the call to the
function in a try/undo block).  It is a runtime error if safety is not
provided.

Ordinary functions (no prefix) cannot call either you functions or
defeat functions, but may be called from anywhere.

Summary of what's allowed in different blocks
.............................................
*(Unless otherwise stated, blocks preserve the rules of the block they are contained by)*

You functions:
 * Function calls: ordinary functions, you functions
 * Conventional control blocks (``while``, ``if``, etc)
 * ``try`` blocks

   * Function calls: ordinary functions, defeat functions
   * Conventional control blocks (``while``, ``if``, etc)
   * ``preempt`` blocks

 * ``undo`` or ``stop`` blocks after ``try``
 * Speculation operator (``??``)

   * Function calls: ordinary functions

Defeat functions:
 * Function calls: ordinary functions, defeat functions
 * Conventional control blocks (``while``, ``if``, etc)
 * ``preempt`` blocks
Ordinary functions:
 * Function calls: ordinary functions
 * Conventional control blocks (``while``, ``if``, etc)
Global scope:
 * Declarations only
