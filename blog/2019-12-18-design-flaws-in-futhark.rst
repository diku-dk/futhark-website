---
title: Design Flaws in Futhark
author: Troels Henriksen
description: Futhark is imperfect, and some of its imperfections are harder to fix than others.
---

Futhark has evolved significantly since its design was first scribbled
on a napkin in late 2012.  The language has since been heavily
modified and extended, and bears little resemblance to the original
prototype.  Features such as `higher-order functions
<2018-04-10-futhark-0.4.0-released.html>`_ and `sum types
<2019-08-21-futhark-0.12.1-released.html>`_ were added as we figured
out how to handle them efficiently.  Since we are pretty much making
all this up as we go along, we get some features wrong initially and
`fix them later <2017-11-11-dot-notation-for-records.html>`_.  Other
features get removed entirely.

For this post, I am going to discuss some aspects of the Futhark
*language* design that I suspect may be flawed, but where the jury is
still out, or where it's not clear what a fix looks like.  I am not
going to talk about missing features (such as recursion) that would
obviously be welcome, but are merely hard to implement well.  I dare
not hope that these are *all* the remaining flaws of Futhark, but they
are the ones I know of.

1-indexing of tuples
====================

In Futhark, components of a tuple can be accessed using the same dot
notation as with records: ``foo.1``, ``foo.2``.  In contrast to
arrays, which are 0-indexed, tuple components are numbered from 1.
While `Dijkstra argues convincingly and correctly for the benefits of
0-indexing
<https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html>`_,
he does so in the context of *ranges*, which does not apply to tuple
indexing.  In Futhark, there is no way to perform arithmetic on a
tuple index.  They must always be constants, because they are really
just names that look like numbers.  Thus the reasoning behind the
inconsistency: 0-indexing is used for arrays because it has benefits
when doing index calculations, while 1-indexing is used for tuples
because it is the more familiar ordering.

Yet, I cannot help but wonder whether we got this one wrong.  How many
programmers are going to accidentally access the wrong field in a
tuple, because they are used to thinking 0-indexed?  We are not even
particularly consistent elsewhere: Futhark numbers OpenCL devices
starting from 0 (because that's what OpenCL itself does), and `our
test runner
<https://futhark.readthedocs.io/en/latest/man/futhark-test.html>`_
also numbers test cases from 0.

The cost of changing this is obvious: every single Futhark program
that uses dot notation for tuples must be modified.  While the
`lightweight anonymous records
<2017-03-06-futhark-record-system.html>`_ means that tuples are not as
widespread as in other languages, I'm not sure the cost is worth it.

32-bit sizes
============

Code emitted by the Futhark compiler uses 32-bit integers to track
array sizes, do index calculations, and so on.  This has some obvious
practical problems (arrays cannot be larger than 2³²-1 elements), but
can lead to substantially faster execution, as both CPUs and GPUs have
substantially lower 64-bit integer performance.  Some time ago, I
experimentally changed the compiler to emit 64-bit index arithmetic
instead, which resulted in a 50% slowdown on an NVIDIA GTX 780Ti GPU
(a somewhat old model) on a `representative benchmark program
<https://github.com/diku-dk/futhark-benchmarks/blob/master/finpar/OptionPricing.fut>`_.

At some point of course, this will stop being an acceptable
constraint.  In principle, addressing this should be as simple as
merely changing the compiler to generate 64-bit index operations, and
then presumably adding some smarts to the code generator to reduce the
overhead as much as possible.  We already use 64-bit pointers and
64-bit allocations, so in principle this is not difficult.  We can
even imagine a compiler option for switching between 32- and 64-bit
index calculations.

Unfortunately, 32-bit sizes have also infiltrated the source language
in a user-visible way.  One of Futhark's few unusual language features
is the support for size annotations, by which functions can express
constraints on the sizes of arrays.  For example, the following
definition of dot product expresses the constraint that the two input
vectors must have the same length:

.. code-block:: Futhark

   let dotprod [n] (xs: [n]f32) (ys: [n]f32) : f32 = ...

However, size parameters such as ``n`` also exist as expression-level
variables, which means they can be used to extract the size of arrays:

.. code-block:: Futhark

   let length [n] 't (arr: [n]t) : i32 = n

Currently, these parameters are always of type ``i32``.  Changing the
type of sizes is likely to break most Futhark programs that use size
parameters in expressions.  In a similar manner, Futhark also allows
value-level parameters to occur in types, which is useful for typing
functions like ``replicate``:

.. code-block:: Futhark

   let replicate 't (n: i32) (x: t) : [n]t = ...

This intermixing means that simply having a compile-time flag to
switch the internal type of sizes is not viable.

One might well ask: should size parameters be in scope at the
expression level at all?  Should we make one large break and change
all sizes to ``i64``, even in the source language?  Maybe sizes should
be an opaque type of an unspecified number of bits?  Perhaps we should
treat sizes as "overloaded", like we do with numeric literals, and
transparently cast sizes to any integer type.  While convenient, it
sounds like a fertile breeding ground for hard-to-test-for bugs.
Something definitely has to be done, but it is not yet clear *what*.

Higher-order modules
====================

Futhark supports an advanced ML-style module system.  The difference
between this and a conventional module system is that beyond simple
name-spacing, ML-style module systems also support *parametric
modules*, by which modules can be parameterised over the concrete
implementation of some other module.  This allows us to program
against interfaces that than implementations, and provides a powerful
mechanism for generic programming.

.. code-block:: Futhark

   -- A module type with an abstract type and two values using that type.
   module type monoid = {
     type t
     val op: t -> t -> t
     val zero: t
   }

   -- Two different implementations of the 'monoid' type.
   module monoid_add_i32 : monoid with t = i32 = {
     type t = i32
     let op = (i32.+)
     let zero = 0i32
   }
   module monoid_prod_f32 : monoid with t = f32 = {
     type t = f32
     let op = (f32.*)
     let zero = 0f32
   }

   -- A parametric module that can generate a "sum" module given any
   -- module that implements the 'monoid' module type.
   module type msum(P: monoid) = {
     let sum (ts: []P.t) : P.t =
       reduce P.add P.zero ts
   }

   -- We can then instantiate the 'msum' module.
   module msum_i32 = msum monoid_add_i32
   module msum_f32 = msum monoid_prod_f32

Parametric modules can be seen as a restricted form of functions at
the module level.  One obvious question is then to ask whether we can
have *higher-order modules* in the same way that we have higher-order
functions.  The answer is *no* for Standard ML, where modules are at
most first-order, but other languages in the ML family, including
Futhark, *do* support higher-order modules.

However, I have come to believe that higher-order modules are a
mistake.  Specifically, compared to their complexity (both
conceptually and implementation-wise), they seem to have very little
utility.  The *only* use of higher-order modules I have seen, outside
the compiler test suite, is an `example program I wrote explicitly to
demonstrate them
<https://github.com/diku-dk/futhark-benchmarks/tree/master/misc/life>`_,
and which I since rewrote in a simpler way.  In contrast to parametric
modules, which are frequently used, the higher-order modules in
Futhark have never since proven useful in practice.

Futhark supports higher-order modules mostly for the research
challenge.  We did get `a paper
<https://futhark-lang.org/docs.html#static-interpretation-of-higher-order-modules-in-futhark>`_
out of it, even including a Coq formalisation of a possible
implementation.  However, the implementation in the compiler is not
mechanically derived from the Coq version, has had several tricky
bugs, and is likely to contain yet more.  The next time I discover a
mysterious failure in the implementation of higher-order modules, I am
likely to just rip it out entirely - especially because the
implementation also complicates the handling of first-order modules.

The issue with higher-order modules is not just one of implementation.
Even conceptually they are hard to understand, and I don't think any
of Futhark's documentation (outside the paper linked above) really
describes them.  ML-style modules are already a fairly big item on the
complexity budget, as they are essentially a distinct sub-language,
and I don't think higher-order modules carry their own weight.

Using ``let`` for both local and top-level definitions
======================================================

In the distant past, Futhark used ``fun`` and ``val`` for top-level
definitions of functions and values, and ``let`` for local bindings
inside a function.  At some point, we realised, no doubt influenced by
OCaml, that we could just use ``let`` in all cases, without any
syntactic confusion.  This was also around the time we added true
function values to the language, so we wanted to remove the old
syntactic distinction between defining "values" and "constants".

Unfortunately, we didn't realise the impact it would have on parse
errors.  As an example, what's wrong with the following program?

.. code-block:: Futhark

   let f x =
     let x' = x + 2

   let g x =
     let x' = x * 2
     in x

Written like this, a human can easily see that ``f`` is missing an
``in``.  However, since ``let g x`` is also syntactically valid as
part of a function definition, the parser will see it like this, and
not report an error until it reaches end-of-file:


.. code-block:: Futhark

   let f x =
     let x' = x + 2
     let g x =
       let x' = x * 2
       in x

There is no clue in the source code that ``g`` is intended as a
top-level function, since local functions have the same syntax.  The
user will be told that an ``in`` was expected but that end-of-file was
reached, and will probably spend a lot of time looking for a problem
with ``g``.

A similar issue occurs when writing editor tooling.  Specifically,
`futhark-mode <https://github.com/diku-dk/futhark-mode>`_ for Emacs
implements automatic indentation the way most Emacs modes do it: a
bunch of regular expressions and crude parser techniques to determine
proper indentation based on the preceding lines.  The advantage of
this approach, compared to running a global re-formatter that uses a
proper parser, is that its effects are local, and work well even for
programs that contain syntax errors.  The problem is that arbitrary
look-back is necessary to determine whether some ``let`` is local (and
so should be indented relative to the enclosing definition) or global,
and so should be indented to column 0.

Both of these issues could have been avoided if we had used a
dedicated keyword, say ``def`` for top-level definitions.  With our
current design, we can certainly work around the issue by making the
parser emit better error messages, such as indicating where the
``let`` missing an ``in`` actually starts.  Maybe we can also improve
futhark-mode to the point where it stops getting confused (or just
switch to an external formatter with an error-tolerant parser).  For
now, we handle it by making the Tab key cycle through multiple
indentation candidates in case of ambiguity.  This is similar to how
Emacs modes tend to handle languages with significant indentation,
such as Python or Haskell, but it's a bit embarrassing that it is
necessary for Futhark just because we neglected to put enough parsing
guideposts into the syntax.
