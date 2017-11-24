---
title: What is Futhark?
---

Futhark is a small programming language designed to be compiled to
efficient parallel code.  It is a **statically typed, data-parallel,
and purely functional array language** in the ML family, and comes
with a **heavily optimising ahead-of-time compiler** that presently
generates GPU code via OpenCL_, although the language itself is
hardware-agnostic.  As a simple example, this function computes the
average of an array of 64-bit floating-point numbers::

  let average (xs: []f64) = reduce (+) 0.0 xs / r64 (length xs)

Futhark is not designed for graphics programming, but instead uses the
compute power of the GPU to accelerate data-parallel array
computations.  The language supports **regular *nested*
data-parallelism**, as well as a form of imperative-style in-place
modification of arrays, while still preserving the purity of the
language via the use of a **uniqueness type system**.

The Futhark language and compiler is an **ongoing research project**.
It can compile nontrivial programs which then run on real GPUs at very
high speed.  The Futhark language itself is still somewhat spartan -
due to the basic design goal of generating high-performance parallel
code, it takes more effort to support language features that are
common in languages with more forgiving compilation targets.
Nevertheless, **Futhark can already be used for nontrivial programs**.
We are actively looking for more potential applications as well as
people who are interested in contributing to the language design.

**Futhark is not intended to replace existing general-purpose
languages**.  The intended use case is that Futhark is only used for
relatively small but compute-intensive parts of an application.  The
Futhark compiler generates code that can be **easily integrated with
non-Futhark code**.  For example, you can compile a Futhark program to
a Python module that internally uses PyOpenCL_ to execute code on the
GPU, yet looks like any other Python module from the outside (`more on
this here`_).  The Futhark compiler will also generate more
conventional C code, which can be accessed from any language with a
basic FFI (`an example here`_).

For more information, you can look at `code examples`_, details on
performance_, our devblog_, or maybe the docs_, which also contains a
list of our publications_.  You can of course also visit our `main
repository on Github`_, or our repository of `benchmarks`_.

.. _OpenCL: https://en.wikipedia.org/wiki/OpenCL
.. _`code examples`: /examples.html
.. _performance: /performance.html
.. _devblog: /blog.html
.. _docs: /docs.html
.. _publications: /docs.html#publications
.. _PyOpenCL: https://mathema.tician.de/software/pyopencl/
.. _associative: https://en.wikipedia.org/wiki/Associative_property
.. _commutative: https://en.wikipedia.org/wiki/Commutative_property
.. _`main repository on Github`: https://github.com/diku-dk/futhark
.. _`more on this here`: /blog/2016-04-15-futhark-and-pyopencl.html
.. _`an example here`: /blog/2017-09-26-calling-futhark-from-c-and-haskell.html
.. _benchmarks: https://github.com/diku-dk/futhark-benchmarks
