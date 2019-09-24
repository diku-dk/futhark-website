---
title: Futhark by Example
---

The following is a hands-on introduction to Futhark through a
collection of commented programs, listed in roughly increasing order of
complexity.  You can load the programs into `the interpreter
<https://futhark.readthedocs.io/en/latest/man/futhark-repl.html>`_ to
experiment with them.  For a conventional introduction to the
language, `Parallel Programming in Futhark`_ may be a better choice.
For more examples, you can check the `examples directory in the
Futhark repository`_, or at our implemented benchmarks_.  We also
maintain a list of `projects using Futhark`_.

.. _`Parallel Programming in Futhark`: https://futhark-book.readthedocs.io
.. _`projects using Futhark`: #projects-using-futhark
.. _`examples directory in the Futhark repository`: https://github.com/diku-dk/futhark/tree/master/examples

* `Basic usage with the factorial function <examples/fact.html>`_

* `Primitive values <examples/values.html>`_

* `Functions <examples/functions.html>`_

* `Arrays <examples/arrays.html>`_

* `Basic parallelism <examples/basic-parallelism.html>`_

* `Tuples and records <examples/tuples-and-records.html>`_

* `Scans and reductions <examples/scan-reduce.html>`_

* `Parametric polymorphism <examples/parametric-polymorphism.html>`_

* `Gather and scatter <examples/gather-and-scatter.html>`_

* `Sum types and pattern matching <examples/sum-types.html>`_

* `Loops <examples/loops.html>`_

* `Benchmarking <examples/benchmarking.html>`_

* `Size parameters <examples/size-parameters.html>`_

* `Matrix multiplication <examples/matrix-multiplication.html>`_

* `Searching <examples/searching.html>`_

* `Computing histograms <examples/histograms.html>`_

* `Radix sort <examples/radix-sort.html>`_

* `Testing for associativity <examples/testing-associativity.html>`_

* `Gaussian blur (with Python integration) <examples/gaussian-blur.html>`_

* `Faking nominal types <examples/nominal-types.html>`_

Projects using Futhark
----------------------

The majority of written Futhark code is probably still Futhark's own
test and benchmark suites.  However, there are some programs that have
been written in Futhark because it was a good tool for the job, and
not just to test the compiler.  A possibly incomplete list:

`Diving Beet <https://github.com/Athas/diving-beet>`_ is a *falling
sand* game, which is a kind of simple particle simulator toy.  Its
main purpose is to produce pretty effects.  There is a `blog post
</blog/2016-12-04-diving-beet.html>`_ with details and a video.

`Futball <https://github.com/Athas/futball>`_ is a game about avoiding
getting hit by balls.  The rendering engine is a ray tracer written in
Futhark.

`Futcam <https://github.com/nqpz/futcam>`_ is an application that
applies stacks of interactively configurable filters to a webcam
stream.  Futhark is used to implement the filters.

`Futracer <https://github.com/nqpz/futracer>`_ is a fairly slow
brute-force ray tracer written in Futhark.

`tail2futhark <https://github.com/henrikurms/tail2futhark>`_ is not
written in Futhark itself, but is a code generator that produces
Futhark, and serves as a component in an APL-to-GPU compilation
pipeline.  There is a `blog post
</blog/2016-06-20-futhark-as-an-apl-compiler-target.html>`_ with more
details.
