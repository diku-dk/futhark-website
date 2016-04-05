---
title: Code Examples
---

Futhark still lacks many of the syntactical niceties that one might
desire in a programming language.  Therefore, the examples here are
more verbose than ideal.  Still, they should provide a taste of what
(simple) Futhark programs look like.  If you have ideas for syntax
improvements, you can always `contribute`_!

As Futhark is a functional language, we will start with the obligatory
factorial program::

  fun int main(int n) = reduce(*, 1, map(1+, iota(n)))

The function call ``fact(n)`` creates an array of the integers
``0..n-1``, adds one to each element of the array, then computes the
product of all elements in the array.  The Futhark compiler employs
*loop fusion* to remove the need for any of these temporary arrays to
be actually created.  Technically ``fact(n)`` does not compute ``n!``,
but rather ``n!  mod 2**32``, as ``int``s are 32 bit in size and will
rapidly overflow for large ``n``.

If we put the above program in a file ``fact.fut``, we can compile it
using the OpenCL backend as such::

  $ futhark-opencl fact.fut

If all goes well, this produces an executable program ``fact`` in the
current directory.  Similarly, we can compile to sequential C code
with ``futhark-c``.  As mentioned in the `introduction`_, Futhark is
not intended to be used for writing standalone programs, but it is
supported in order to enable testing and benchmarking.  A standalone
program will expect to be given its arguments on standard input::

  $ echo 2000000000 | ./fact

This may produce a bunch of diagnostics on standard error, but will
also cause the result (``0i32`` - a 32-bit zero) to be written on
standard output.  We can use built-in instrumentation to determine how
long the computation took, not counting GPU context initialisation and
the like::

  $ echo 2000000000 | ./fact -t runtime.txt

The file ``runtime.txt`` will contain the wall time in microseconds.
On a GTX 780 Ti GPU will compute ``fact(2000000000)`` (two billion) in
7.0ms .  A `sequential C program`_ using a ``for``-loop to compute the
same thing takes 1335.3ms on an Intel Xeon E5-2650 CPU.  Of course,
this is not a realistic performance comparison, as neither program
accesses memory, but it shows how easy it is to obtain parallel
execution in Futhark.  If we ask the Futhark compiler to generate
sequential C code (with ``futhark-c``), the resulting program runs in
exactly the same time as the hand-written C program.

A More Complex Example
**********************

A more interesting example is the *maximum segment sum problem*
(*MSS*), where we wish to determine the maximum sum of any contiguous
subsequence of an array of integers.  We can implement this in Futhark
using a combination of ``map`` and ``reduce``::

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
problems (such as *MSS* above) cannot be solved efficiently with just
commutative reductions.

On a GTX 780 Ti GPU, Futhark can compute the MSS of ten million
integers in 1.4ms.

.. _`contribute`: /getinvolved.html
.. _`introduction` /
.. _`sequential C program` /static/sequential-fact.c
