==============
Halt is Defeat
==============

Halt is Defeat is a prophecy-optimized language targeting the Sphinx
ISA.

Key features of Halt is Defeat:
 * Function overloading
 * Time-travel semantics
 * Stack-allocated variable-length arrays
 * Type coercion and inference of ambiguous expressions
 * Memory safety through bounds checking and stack-overflow detection
 * Safety from defeat
 * Tail-call elimination (TODO)
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
The entry point to Halt is Defeat programs is ``@is_you()``.
There are 3 kinds of function in Halt is Defeat, ``@is_you()`` being a
"you" function.  You can tell because the name starts with an ``@``, but
for now you may ignore this distinction.  Here's a simple program:

.. code::

    void @is_you() {
        println("Hello world!");
        print("Some numbers:");
        for (int i = 1; i <= 10; i += 1) {
            print(' ');
            print(i);
        }
        println();
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

A taste of defeat
-----------------

Defeat functions have names starting with ``!``, and may result in
defeat if called.  ``!is_defeat()`` always results in defeat.  Defeat
functions cannot be called directly from within you functions, but you
can create a try block within you to safely run code which may lead to
defeat.

In this example we use the ``catch`` handler, which treats defeat
similarly to exceptions in other languages.  The ``catch`` block is run
when defeat is reached:

.. code::

    void @is_you() {
        try {
            println("try block");
            !is_defeat();
        } catch {
            println("catch block");
        }
    }

Output:

.. code::

    $ hidc catch.hid -o catch.s
    $ spasm catch.s
    try block
    catch block
    Reached win flag
        CPU time: 177 clock cycles
        Emulator efficiency: 46.21%


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

The ``undo`` handler works similarly to ``catch``, but it goes back in
time and prevents the ``try`` block from running if it would result in
defeat:

.. code::

    void @is_you() {
        try {
            println("try block");
            !is_defeat();
        } undo {
            println("undo block");
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

    void @is_you() {
        try {
            println("The loop runs forever");
            while (true) {}
            !is_defeat();
        } undo {
            println("The loop terminates");
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


Other features
==============

Command-line arguments
----------------------
Halt is Defeat makes use of Sphinx's robust argument specifiers, which I
added to Sphinx mostly so that Halt is Defeat could make use of them.

If you want command-line arguments, you can write your ``@is_you``
function with the signature ``void @is_you(const string[] args)``

Does your program take integers as input?  Don't want to write code to
parse them?  Don't worry!  You can get ``spasm`` to do it for you!
The signature ``void @is_you(const int[] args)`` specifies that the
inputs should be integers, which ``spasm`` will be parse (in base 10)
from the command line arguments.

You can even mix and match:
``void @is_you(string mode, const int[] args)``

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

    void @is_you(const string[] args) {
        string mutargs[args.length];
        for (int i = 0; i < args.length; i += 1) {
            mutargs[i] = args[i];
        }
    }

Increasing the word size and stack size
---------------------------------------

By default, ``hidc`` targets a 16-bit word size, and provides a
picture's worth (1000 words) of stack space.
These can both be increased.

- To change the word size, use ``-m``, eg ``-m24`` to target 24 bits.
- To change the stack size, use ``-s``, eg ``-s2000`` for 2000 words.

As an alternative to increasing the stack size, you may also consider
making your variables/arrays global, and where possible making them
const.

Although increasing the word and stack size can increase the size of the
problems you can solve with HiD, be wary of the exponential tendencies
of emulation under ``spasm`` -- you may want to take things slow.
There's no prize for writing a program that requires more RAM in order
to emulate than could fit in the observable universe, it just means you
need a better computer.

If using 24 bit words, I recommend decreasing the stack size to 100
words, at least in testing, so that if you accidentally write a 24-bit
loop ``spasm`` won't need more than 8 GB should that loop need to be
predicted (assuming no globals).  If using more than 24 bits, you are on
your own, may god have mercy.

Fatal errors and undefined behavior
-----------------------------------

Fatal errors occur when invalid operations are performed, such as
dividing by 0.  Errors are different from defeat, and in fact provide
safety from defeat similar to the ``win`` state.  As a result of this,
the path of execution leading up to an error might not have actually
occurred if the conditions that produced the error were fixed.  For
example, in a try/undo block you might have a path of execution which
"should" lead to defeat, but instead causes a stack overflow.  This
could cause code to run which otherwise wouldn't if the stack size were
increased, possibly printing "incorrect" output leading up to the error.

This can be confusing, but it is much more useful in debugging the
causes of an error than if errors were to cause defeat.

Errors can also be produced in user code with ``all_is_broken()``.

Although many operations in HiD are checked and will produce errors, the
following are undefined behavior:

- Accessing uninitialized strings in dynamically allocated arrays
- Dynamically allocating an array with negative length (usually this
  will be caught by stack-overflow detection, but not necessarily)

Additionally, if the ``--unchecked`` flag is passed, all previously
checked operations become undefined behavior:

- Division by 0
- Indexing an array out of bounds
- Stack overflow

Be aware that HiD's nasal demons can time travel, so undefined behavior
may result in a program's defeat before it even starts, etc.
