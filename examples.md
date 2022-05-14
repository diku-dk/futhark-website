---
title: Futhark by Example
---

The following is a hands-on introduction to Futhark through a
collection of commented programs, listed in roughly increasing order
of complexity.  You can load the programs into [the
interpreter](https://futhark.readthedocs.io/en/stable/man/futhark-repl.html)
to experiment with them. For a conventional introduction to the
language, [Parallel Programming in
Futhark](https://futhark-book.readthedocs.io) may be a better choice.
For more examples, you can check our implemented
[benchmarks](https://github.com/diku-dk/futhark-benchmarks). We also
maintain a list of [projects using Futhark](#projects-using-futhark).

Some of the example programs use
[directives](https://futhark.readthedocs.io/en/latest/man/futhark-literate.html)
for plotting or rendering graphics.

# Basic language features

-   [Basic usage with the factorial function](examples/fact.html)
-   [Primitive values](examples/values.html)
-   [Converting a value to a different type](examples/converting.html)
-   [Functions](examples/functions.html)
-   [Arrays](examples/arrays.html)
-   [Type ascriptions](examples/type-ascriptions.html)
-   [Basic parallelism](examples/basic-parallelism.html)
-   [Tuples and records](examples/tuples-and-records.html)
-   [Shape coercions](examples/shape-coercions.html)
-   [Ranges](examples/ranges.html)
-   [Scans and reductions](examples/scan-reduce.html)
-   [Parametric polymorphism](examples/parametric-polymorphism.html)
-   [Gather and scatter](examples/gather-and-scatter.html)
-   [Pipe operators](examples/piping.html)
-   [Complex ranges](examples/complex-ranges.html)
-   [Sum types and pattern matching](examples/sum-types.html)
-   [Loops](examples/loops.html)

# Programming techniques

-   [Benchmarking](examples/benchmarking.html)
-   [Counting elements that satisfy
    property](examples/filter-length.html)
-   [Reducing the result of a filter](examples/filter-reduce.html)
-   [Scattering the result of a filter](examples/filter-scatter.html)
-   [Size parameters](examples/size-parameters.html)
-   [Matrix multiplication](examples/matrix-multiplication.html)
-   [Pairwise L₁ distances](examples/pairwise-l1.html)
-   [Outer product](examples/outer-product.html)
-   [Searching](examples/searching.html)
-   [Computing histograms](examples/histograms.html)
-   [Moving average](examples/moving-average.html)
-   [Radix sort](examples/radix-sort.html)
-   [Abstract data types](examples/abstract-data-types.html)
-   [Testing for associativity](examples/testing-associativity.html)
-   [Reducing or scanning without a neutral
    element](examples/no-neutral-element.html)
-   [Holes](examples/holes.html)
-   [Kahan summation](examples/kahan.html)
-   [Random numbers](examples/random-numbers.html)
-   [Gaussian blur (with Python
    integration)](examples/gaussian-blur.html)
-   [Three-dimensional vectors](examples/3d-vectors.html)
-   [Faking nominal types](examples/nominal-types.html)
-   [Triangular matrices](examples/triangular.html)
-   [Binary search](examples/binary-search.html)
-   [AD with dual numbers](examples/dual-numbers.html)

# Automatic differentiation

-   [Forward-mode automatic differentiation](examples/forward-ad.html)
-   [Reverse-mode automatic differentiation](examples/reverse-ad.html)
-   [Newton's Method](examples/newton-ad.html)

# Literate Futhark

-   [Basic use of literate Futhark](examples/literate-basics.html)
-   [Generating videos with literate Futhark](examples/literate-video.html)

# Examples from Dex

The following examples are ported from
[Dex](https://github.com/google-research/dex-lang), a dependently typed
functional array language that uses a somewhat different approach to
describing loop processing. We\'ve tried to keep the original naming
scheme and programming style.

-   [Prelude](examples/dex-prelude.html)
-   [Mandelbrot set](examples/dex-mandelbrot.html)
-   [Multi-step ray tracer](examples/dex-raytrace.html)
-   [Monte Carlo estimates of pi](examples/dex-pi.html)
-   [Brownian motion](examples/dex-brownian-motion.html)
-   [Sierpinski triangle](examples/dex-sierpinski.html)

# Projects using Futhark

The majority of written Futhark code is probably still Futhark\'s own
test and benchmark suites. However, there are some programs that have
been written in Futhark because it was a good tool for the job, and not
just to test the compiler. A possibly incomplete list:

[Diving Beet](https://github.com/Athas/diving-beet) is a *falling sand*
game, which is a kind of simple particle simulator toy. Its main purpose
is to produce pretty effects. There is a [blog
post](/blog/2016-12-04-diving-beet.html) with details and a video.

[Futball](https://github.com/Athas/futball) is a game about avoiding
getting hit by balls. The rendering engine is a ray tracer written in
Futhark.

[Futcam](https://github.com/nqpz/futcam) is an application that applies
stacks of interactively configurable filters to a webcam stream. Futhark
is used to implement the filters.

[Futracer](https://github.com/nqpz/futracer) is a fairly slow
brute-force ray tracer written in Futhark.

[Futswirl](https://github.com/nqpz/futswirl) is a fractal generator
based on [iterated function
systems](https://en.wikipedia.org/wiki/Iterated_function_system).

[Neptune](https://github.com/filecoin-project/neptune) is an
implementation of the [Poseidon hash
function](https://www.poseidon-hash.info/) tuned for
[Filecoin](https://filecoin.io/), where [the GPU
parts](https://github.com/filecoin-project/neptune-triton) have been
implemented in Futhark.

[Palathark](https://githepia.hesge.ch/orestis.malaspin/palathark) is a
Futhark implementation of the lattice Boltzmann method.

[Ray Tracing in One Weekend in
Futhark](https://github.com/athas/raytracinginoneweekendinfuthark) and
[Ray Tracing: the Next Week in
Futhark](https://github.com/athas/raytracingthenextweekinfuthark/) are
implementations based on Peter Shirley\'s book series. These are by no
means real-time ray tracers, but support advanced effects and make use
of acceleration structures like BVH trees.

[Pareas](https://github.com/Snektron/pareas) is a compiler implemented
in Futhark and C++.
