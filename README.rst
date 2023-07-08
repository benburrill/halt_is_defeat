==============
Halt is Defeat
==============

Halt is Defeat is a prophecy-optimized language targeting the Sphinx
ISA.

Key features of Halt is Defeat:
 * Function overloading
 * Stack-allocated variable-length arrays
 * Overly-complicated type coercion rules
 * Memory safety through bounds checking
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
then enters into terminal non-termination.

If running under the ``spasm`` emulator, you can always issue a keyboard
interrupt (ctrl-c) to forcibly stop the running program.  Attempting to
stop programs running on a real Sphinx processor is not recommended and
may destroy the universe.

A taste of defeat
-----------------

.. code::

    void @is_you() {
        try {
            println("try block");
            !is_defeat();
        } catch {
            println("catch block");
        }
    }


Time travel
-----------
Sure, it's nice to know when you've made a mistake.  But more often than
not, we really just wish we could go back and undo it.  Halt is Defeat
provides an undo block for this purpose.

.. code::

    void @is_you() {
        try {
            println("try block");
            !is_defeat();
        } undo {
            println("undo block");
        }
    }


Halting problems
----------------
The undo block allows us to do some rather interesting things.  If we
modify the above code by putting a loop before the ``!is_defeat()``, the
code will test if the loop will terminate, since defeat would never
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
It is "Turing-complete-ish" (just like your computer is), but that only
means its halting problem is generally intractable, not undecidable.

Sphinx's entire execution is based around this.  The instruction set
provides only a single jump instruction, the "Turing jump instruction",
which performs a jump if not jumping would lead to halting.


Computational astrology
-----------------------
