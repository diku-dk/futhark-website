---
title: What is Futhark?
---

Futhark is a small programming language designed to be compiled to
highly performant GPU code.  It is a **statically typed,
data-parallel, and purely functional array language**, and comes with
a **heavily optimising ahead-of-time compiler** that generates GPU
code via OpenCL_.  Futhark is not designed for graphics programming -
rather, it uses the compute power of the GPU to accelerate
computations on data-parallel array computations.  We support
**regular *nested* data-parallelism**, as well as a form of
imperative-style in-place modification of arrays, while still
preserving the overall purity of the language via the use of a
**uniqueness type system**.

The Futhark language and compiler is an **ongoing research project**.
It is capable of compiling nontrivial programs which can then run on
real GPUs at very high speed.  The Futhark language itself is still
very spartan - due to the basic design criteria requiring the ability
to generate high-performance GPU code, it takes more effort to support
language features that are common in languages with more forgiving
compilation targets.

Nevertheless, Futhark can already be used for real-world programming.
**Futhark is not intended to replace your existing languages**.  Our
intended use case is that Futhark is only used for relatively small
but compute-intensive parts of the overall application.  The Futhark
compiler then generates code that can be **easily integrated with
non-Futhark code**.  For example, you can compile a Futhark
program to a Python module that internally uses PyOpenCL_ to execute
code on the GPU, yet looks like any other Python module from the
outside.  The Futhark compiler will also generate more conventional C
code, which can be accessed from any language with a basic FFI.

For more information, you can look at the page on performance_, on our
devblog_, or maybe the docs_.

Examples
--------

Futhark still lacks many of the syntactical niceties that one might
desire in a programming language.  Therefore, the examples here are a
little bit more verbose than ideal.  Still, they should provide a
taste of what (simple) Futhark programs look like.

As Futhark is a functional language, we will start with the obligatory
factorial program::

  fun int main(int n) = reduce(*, 1, map(1+, iota(n)))

The function call ``fact(n)`` creates an array of the integers
``0..n-1``, adds one to each element of the array, then computes the
product.  The Futhark compiler employs *loop fusion* to remove the
need for any of these temporary arrays to be actually created.  Also,
technically ``fact(n)`` does not compute ``n!``, but rather ``n!  mod
2**32``, as ``int``s are 32 bit in size and will rapidly overflow for
large ``n``.

When compiled with the OpenCL backend, the above function will compute
``fact(2000000000)`` (two billion) in 7.0ms on a GTX 780 Ti GPU.  A
sequential C program using a ``for``-loop to compute the same thing
takes 1335.3ms on an Intel Xeon E5-2650 CPU.  Of course, this is not a
realistic performance comparison, as neither program accesses memory,
but it shows how easy it is to obtain parallel execution in Futhark.
As an interesting sidenote, if we ask the Futhark compiler to generate
sequential C code, the resulting program runs in exactly the same time
as the hand-written C program.

A more interesting example is the *maximum segment sum problem*, where
we wish to determine the maximum sum of any contiguous subsequence of
an array of integers.  We can implement this in Futhark using a
combination of ``map`` and ``reduce``::

  fun int max(int x, int y) =
    if x > y then x else y

  fun {int,int,int,int} redOp({int,int,int,int} x,
                              {int,int,int,int} y) =
    let {mssx, misx, mcsx, tsx} = x in
    let {mssy, misy, mcsy, tsy} = y in
    { max(mssx, max(mssy, mcsx + misy))
    , max(misx, tsx+misy)
    , max(mcsy, mcsx+tsy)
    , tsx + tsy }

  fun {int,int,int,int} mapOp (int x) =
    { max(x,0), max(x,0), max(x,0), x }

  fun int main([int] xs) =
    let {x, _, _, _} =
      reduce(redOp, {0,0,0,0}, map(mapOp, xs)) in
    x

Note that Futhark uses curly braces for tuples.  One interesting
aspect about this program is that it involves a reduction with an
operator that is associative_, but not commutative_.  Associativity is
a requirement for the parallel execution of reductions, but
commutativity is not required.  Yet, for reasons of implementation
difficulty, many parallel languages and libraries will malfunction if
the reduction operator is not commutative.  Futhark supports
non-commutative operators, as we have found that many interesting
problems (such as *maximum segment sum* above) cannot be solved
efficiently with just commutative reductions.

.. _OpenCL: https://en.wikipedia.org/wiki/OpenCL
.. _performance: /performance.html
.. _devblog: /blog.html
.. _docs: /docs.html
.. _PyOpenCL: https://mathema.tician.de/software/pyopencl/
.. _associative: https://en.wikipedia.org/wiki/Associative_property
.. _commutative: https://en.wikipedia.org/wiki/Commutative_property
