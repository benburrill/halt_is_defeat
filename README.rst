==============
Halt is Defeat
==============

Halt is Defeat is a prophecy-optimized oracle-oriented programming
language targeting the Sphinx ISA.

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

If you run it yourself, you may notice something a little odd.  Once it
reaches the end of the program, it gets stuck in an infinite loop.

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
