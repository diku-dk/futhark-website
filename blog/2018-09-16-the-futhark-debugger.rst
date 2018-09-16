---
title: The Futhark Debugger
author: Troels Henriksen
description: We implemented some tooling to make it easier to debug faulty Futhark programs.
---

Programs have bugs.  It is important that programming language
implementations provide some means of detecting and correcting them.
These means can take many forms.  Low-level debuggers such as ``gdb``
and ``lldb`` operate at the level of assembly instructions;
highlighting the triggering instruction in the event of segmentation
faults, and providing access to register and memory contents.  In
practice, of course, compilers insert various annotations in the
compiled program to help map instructions, registers, and memory
addresses back to program statements and variables.  This is feasible
only if the compiled code still bears significant similarity to the
original source—for example by disabling some optimisations.  For
languages that are intrinsically distant from the machine, however,
even basic compilation results in machine code that requires major
detective work to map back to what the programmer wrote.

Futhark is in this latter category.  For such high-level languages,
other techniques are used.  The simplest, of course, is to litter the
program with print statements and thus divine the internal state of an
erroneous execution.  Such "``printf`` debugging" is likely well known
to anyone reading this post, and is a surprisingly effective technique
that requires no extra tooling.  Indeed, it is the main technique by
which I debug the Futhark compiler itself, through the use of the
Haskell function `Debug.Trace.trace
<http://hackage.haskell.org/package/base-4.11.1.0/docs/Debug-Trace.html#v:trace>`_.
Unfortunately, this technique is not directly applicable to Futhark,
since Futharks compilation model assumes absolute purity—and the
language itself is ill suited for constructing strings.

One problem with debugging compiled Futhark code is that the compiler
aggressively reorganises not just the code, but also the data
representation.  As a simple example, an array of pairs
``[](i32,i32)`` is represented at run-time as a pair of arrays
``([]i32, []i32)``.  Other arrays may disappear entirely, being fused
into whichever operations use them.  And finally, Futhark does not
even mandate an element ordering in memory, and will represent the
dimensions of a multidimensional array as whatever is most efficient
for how it is eventually accessed.  The actual layout of the array is
not stored anywhere, but is embedded directly into the generated loops
that access it.  This is the downside of a heavily optimising
compiler: the resulting code can be completely inscrutable.

High-level languages typically provide an interactive environment
known as a `REPL
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`_—
a Read-Eval-Print-Loop.  This is a powerful debugging tool because it
allows the programmer to invoke internal functions in an ad-hoc manner
and inspect their results.  Typically, but not always, REPLs are built
on top of an interpreter, as this provides faster response times.

Futhark has possessed an interpreter since the `very early days
<https://github.com/diku-dk/futhark/blob/c3ded8f47e87d0e0e6a81d875acafc8c178eb7a5/src/L0/Interpreter.hs>`_
(before the language was even called Futhark!).  The interpreter was
used to nail down the initial semantics before we had a code
generator.  Eventually, Futhark was split between the source language
and a desugared "core" language.  For reasons I no longer recall, the
interpreter was chosen to operate on the core language.  As the source
and core languages continued to diverge—especially as the latter
became dependently typed—we eventually had a situation where there
was no longer and straightforward reference implementation of the
source language.  The core language interpreter had to pass the source
program through a range of nontrivial compiler passes, for example the
array representation transformation, before it was able to execute the
program.  While we did cobble together a REPL using this interpreter,
it was clumsy and reported results in a desugared representation that
was not necessarily compatible with the type of the original program.

The New Interpreter
-------------------

As a result, I took a couple of days in late August to fashion a new
interpreter that operates directly on a type-checked source language
program.  Performance was not a concern—we have compilers for that—so
it was straightforward to write.  To enable invasive debugging
techniques like tracing and breakpoints, while keeping the
implementation simple and pure, I constructed it using a `free monad
<http://hackage.haskell.org/package/free-5.1/docs/Control-Monad-Free-Church.html>`_
that allows the introduction of arbitrary effects, which are then
handled by an external layer.

The following is an example of how to use the new interpreter,
``futharki``, to inspect Futhark code.  Since ``futharki`` is very
slow when compared to compiled code, we can only debug on cut-down
smaller testing sets, not on realistic workloads.  However, we
conjecture that in data-parallel programs, erroneous behaviour does
not depend on the size of the input—we have "bug scalability", in a
sense.  I think that data-parallelism is unusual among parallel
programming paradigms by having this property.

``futharki`` exposes debugging facilities through two special
functions: ``trace`` and ``break``.  The ``trace`` function has the
following type::

  trace 't : t -> t

Semantically, ``trace`` just returns its argument unchanged, and when
compiling your Futhark code, this is indeed all that will happen.
However, ``futharki`` treats ``trace`` specially, and will print the
argument to the screen.  This is useful for seeing the value of
internal variables.  For example, suppose we have the program
``trace.fut``::

  let main (xs: []i32) = map (\x -> trace x + 2) xs

We can then run it with ``futharki`` to get the following output:

.. code-block:: none

   $ echo [1,2,3] | futharki trace.fut
   Trace at trace.fut:1:24-1:49: 1i32
   Trace at trace.fut:1:24-1:49: 2i32
   Trace at trace.fut:1:24-1:49: 3i32
   [3i32, 4i32, 5i32]

Similarly, the ``break`` function is semantically also the identity function::

  break 't : t -> t

When ``futharki`` encounters ``break``, it suspends execution and lets
us inspect the variables in scope.  At the moment, this works *only*
when running an expression within the ``futharki`` REPL, *not* when
running directly from the command line.  Suppose ``break.fut`` is::

  let main (xs: []i32) = map (\x -> break x + 2) xs

Then we can load and run it from ``futharki``:

.. code-block:: none

   > :load break.fut
   > main [1,2,3]
   Breaking at > :1:1-1:12 -> break.fut:1:24-1:49 -> /futlib/soacs.fut:35:3-35:24 -> break.fut:1:35-1:41.
   <Enter> to continue.
   > x
   1i32
   > x*10
   10i32
   >
   Continuing...
   Breaking at > :1:1-1:12 -> break.fut:1:24-1:49 -> /futlib/soacs.fut:35:3-35:24 -> break.fut:1:35-1:41.
   <Enter> to continue.
   >
   Continuing...
   Breaking at > :1:1-1:12 -> break.fut:1:24-1:49 -> /futlib/soacs.fut:35:3-35:24 -> break.fut:1:35-1:41.
   <Enter> to continue.
   >
   Continuing...
   [3i32, 4i32, 5i32]
   >

Whenever we are stopped at a break-point, we can enter arbitrary
Futhark expressions to inspect the state of the environment.  This is
useful when operating on complex values, where we might want to
inspect only a small part.  We are even able to call locally declared
functions if we so wish!

The Underlying Philosophy
-------------------------

The `desert survival strategy
<2018-06-18-designing-a-programming-language-for-the-desert.html>`_
also affected the design of these debugging facilities.  In
particular, they had to be easy to maintain and easy to learn for
users.  Increasing the maintenance burden was avoided by removing the
old interpreter.  In fact, the old interpreter was problematic in that
we had to modify it whenever we added or removed constructs in the
core language, which happens much more frequently than in the source
language.

From the user's point of view, I did not want to add a Futhark
debugging tool with its own set of commands that had to be memorized.
However, every Futhark programmer knows how to call functions, and
(eventually) knows how to enter expressions at a REPL.  Hence, the
debugger interface was built to depend almost entirely on these two
concepts.  This is in contrast to, say, ``gdb``, where you usually set
breakpoints via debugger commands, and inspect the environment via
various printing commands (although in fairness, ``gdb`` also allows
some C expressions to be entered).

I have already used ``futharki`` to debug various programs, and for
all its simplicity, it seems to work very well.  Most of all, I look
forward to being answer the question *"how do I debug my program?"*
with something else than *"stare really hard at it for a few hours"*.
In retrospect, I'm a little surprised we've managed to get things
working at all.

Futhark is a small and simple language—only the compilers are large
and complicated.  It has been our hope that this would mean that it is
easy to write tools for the language.  I believe this Futhark
interpreter provides some evidence that this is indeed the case.
