---
title: Gotta Go Fast!
---

There is no good way to define what it means for a language to be
*fast*.  While some benchmark suites exist, such as the `benchmarks
game`_, they are typically designed for general-purpose CPU languages,
and thus not a natural fit for Futhark.

This page will not try to quantitatively prove that Futhark is the
fastest language ever, but instead attempt to give an intuition of
where Futhark lives in the performance space.  This is done by
comparing performance on fairly simple programs to hand-written
implementations or other high-quality GPU languages or libraries.

The graphs show the input size on the *x*-axis, and the resulting
runtime in microseconds on the *y*-axis.  All runtimes are averaged
over a hundred runs.  For reference, we also include the runtime for
Futhark compiled to sequential CPU code.

Sum (`Futhark code <benchmarks/programs/sum.fut>`_, `Thrust code <benchmarks/programs/sum.cu>`_)
================================================================================================

.. image:: images/sum.svg
   :alt: Summation runtime (lower is better)

This program simply sums an array of integers.  We compare the Futhark
code to Thrust_, which is a very high-quality C++ library developed by
NVIDIA (now maintained as an open source project) for GPU programming
in a high-level STL-like style.  This program lives in a Thrust sweet
spot, as it can be expressed with a single reduction with a simple
operator.  Thrust has well-implemented basic primitives, which means
that matching it in performance on simple programs is a satisfying
result.

MSS (`Futhark code <benchmarks/programs/mss.fut>`_, `Thrust code <benchmarks/programs/mss.cu>`_)
================================================================================================

.. image:: images/mss.svg
   :alt: MSS runtime (lower is better)

This graph shows performance of a *maximum segment sum* implementation
in Futhark and Thrust.  The sequential runtime is for Futhark code
compiled to sequential CPU code.  This example is interesting because
it involves a non-commutative reduction.  In Thrust, we have to
implement this via an inclusive scan followed by picking out the last
element.  This is not too problematic for small input sizes, but the
extra amount of work begins to have an impact for larger datasets.

.. _`benchmarks game`: https://benchmarksgame.alioth.debian.org/
.. _`Thrust`: https://github.com/thrust/thrust`
