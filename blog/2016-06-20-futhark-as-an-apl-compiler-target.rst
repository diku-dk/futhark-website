---
title: Futhark as target language for an APL compiler
author: Troels Henriksen
description: We have been experimenting with using Futhark as an optimisation and code generation backend for a research APL compiler.
---

Futhark is an array programming language - a family of languages with
little widespread usage today, unless we count languages such as
Matlab, R, or perhaps the Numpy library for Python.  Despite their
relative obscurity today, array languages are one of the oldest
language families around.  `Ken Iverson`_ started design on his `APL
programming language`_ in 1957, although it took a few more years
before it was fully specified and implemented.  The 1979 Turing Award
was awarded to Iverson for his work on APL.

APL is most famous for being cryptic and concise, which is partially
due to its use of a unique character set.  For example, to sum an
array ``A`` with its transpose and then sum the rows, we would say::

  +/A+⍉A

If one looks a little deeper, APL actually has very straightforward
syntactic rules and is quite easy to learn.  However, this post is not
about programming in APL - it is about *implementing* APL.

APL and related languages are typically implemented with interpreters.
This is partially due to APL being dynamically typed, and partially
due to array languages able to get decent performance if the
interpretative overhead is amortised by an efficient implementation of
array primitives.  One APL variant, the `K programming language`_, is
famous for its performance (and perhaps even more cryptic syntax).
However, the interpreter approach does have its limitations.  In the
above example, both the transposed array ``⍉A`` and the sum ``A+⍉A``
will be created in memory, even though they are merely temporary
values that end up being consumed by the reduction (the ``+/`` part).
In Futhark, we would write it like this::

  fun []int main([n][n]int a) =
    let a_t = transpose(a)
    let a_sum = zipWith(fn [n]int ([]int r0, []int r1) =>
                          zipWith(+, r0, r1),
                        a, a_t)
    in map(fn int ([]int r) =>
             reduce(+, 0, r),
          a_sum)

This is significantly more verbose.  However, the Futhark compiler
employs *loop fusion* to combine the different operations into a
single efficient loop, and is able to turn the entire program into GPU
code.  Ideally, we want the concision of APL, but with the performance
that can be achieved with an optimising compiler.

One APL compiler project is `apltail`_, which is based on translating
programs written in (a subset of) APL into a *typed array intermediate
language* (TAIL).  TAIL is a fairly conventional statically typed
functional language, where the implicit behaviour of APL has been made
explicit.  From there, it turns out to be a relatively straightforward
task to implement a `compiler from TAIL to Futhark`_, which ultimately
results in a compilation pipeline from APL to optimised GPU code.  We
have summarised our work in a paper submitted to `FHPC'16`_ (`preprint
<publications/fhpc16.pdf>`_).

This work is interesting for two reasons: first, it enables a level of
high-performance programming in APL that has been hard to achieve
before.  Second, it serves as a demonstration of Futhark's suitability
as a *compiler target* (in contrast to human-written code).  Such a
demonstration requires not only sufficient language expressivity, but
also that the code produced by the translation is able to be optimised
approximately as well as hand-written code.  This is particularly
important for array languages, where large-scale structural
transformations (like fusion) are of critical importance for
performance.

While the user experience is still rather raw, we hope that this work
can serve as the first step to using Futhark as a back-end optimiser
for various different languages.

.. _`Ken Iverson`: https://en.wikipedia.org/wiki/Kenneth_E._Iverson
.. _`APL programming language`: https://en.wikipedia.org/wiki/APL_%28programming_language%29
.. _`K programming language`: http://kparc.com/
.. _`apltail`: https://github.com/melsman/apltail/
.. _`compiler from TAIL to Futhark`: https://github.com/henrikurms/tail2futhark
.. _`FHPC '16`: https://sites.google.com/site/fhpcworkshops/fhpc-2016
