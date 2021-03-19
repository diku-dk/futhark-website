---
title: Gotta Go Fast!
---

There is no good way to define what it means for a language to be
*fast*. While some benchmark suites exist, such as the [benchmarks
game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/),
they are typically designed for general-purpose CPU languages, and not a
natural fit for Futhark.

This page will not try to quantitatively prove that Futhark is the
*fastest language ever*. Instead, it merely attempts to give an
intuition of where Futhark lies in the performance space. This is done
by comparing performance on fairly simple programs to hand-written
implementations or other high-quality GPU languages or libraries.
Unfortunately, such comparisons mostly exercise the efficiency of basic
language constructs. These programs are too small and simple to benefit
from the ability of the Futhark compiler to significantly restructure
the original program and its data representations through *loop fusion*
and the like. More comprehensive benchmarks can be found in our various
[papers](/docs.html).

The graphs show input size on the *x*-axis, and the resulting runtime in
microseconds on the *y*-axis. All runtimes are averaged over a hundred
runs. For reference, we also include the runtime for Futhark compiled to
sequential CPU code. The benchmarking setup and code can be found
[here](https://github.com/diku-dk/futhark-website/tree/master/benchmarks).
There is also the [full Futhark benchmark
suite](https://github.com/diku-dk/futhark-benchmarks) and its
[dashboard](https://futhark-lang.org/benchmark-dashboard), which shows
performance changes over time.

# Sum ([Futhark](benchmarks/programs/sum.fut), [Thrust](benchmarks/programs/sum.cu))

![Summation runtime (lower is better)](images/sum.svg){.performance_graph
.performance_graph}

This program simply sums an array of integers. We compare the Futhark
code to [Thrust](https://github.com/thrust/thrust), a C++ library
developed by NVIDIA (now maintained as an open source project) for GPU
programming in a high-level STL-like style. This program resides in a
Thrust sweet spot, as it can be expressed as a single reduction with a
simple operator. Thrust has well-implemented basic primitives, which
means that matching it in performance on simple programs is a satisfying
result. The sequential runtime is for Futhark code compiled to
sequential CPU code.

# MSS ([Futhark](benchmarks/programs/mss.fut), [Thrust](benchmarks/programs/mss.cu))

![MSS runtime (lower is better)](images/mss.svg){.performance_graph
.performance_graph}

This graph shows performance of a *maximum segment sum* implementation
in Futhark and Thrust. This example is interesting because it involves a
non-commutative reduction. In Thrust, we have to implement this via an
inclusive scan followed by picking out the last element. This is not too
problematic for small input sizes, due to the high quality of the scan
implementation in Thrust, but the extra work begins to have an impact
for larger input sizes.

# Mandelbrot ([Futhark](benchmarks/programs/mandelbrot.fut), [Thrust](benchmarks/programs/mandelbrot.cu), [Accelerate](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/mandelbrot))

![Mandelbrot runtime (lower is better)](images/mandelbrot.svg){.performance_graph
.performance_graph}

[Accelerate](https://github.com/AccelerateHS/accelerate) is an array
language embedded in Haskell that supports flat data-parallelism, and
which comes with a backend for NVIDIA GPUs. The design of Accelerate
makes it natural to jump from CPU to GPU code within a single Haskell
program, although that is not used in this benchmark. When visualising
the Mandelbrot set, each pixel can be computed completely independently
and with no memory accesses apart from the final write, making it an
excellent fit for GPU computing. The amount of work done is proportional
to the square of the value on the *x*-axis. Rendering the fractal to an
image is not included in the runtime measurement, but the visualisation
looks like [this](images/mandelbrot1000.png).

# HotSpot ([Futhark](benchmarks/programs/hotspot.fut), [Rodinia](http://rodinia.cs.virginia.edu/doku.php?id=hotspot))

![HotSpot runtime (lower is better)](images/hotspot.svg){.performance_graph
.performance_graph}

The [Rodinia](http://rodinia.cs.virginia.edu/doku.php) suite is a
collection of hand-written benchmark programs that exercise various
forms of GPU computation. One of these programs is HotSpot, which
Rodinia describes as:

> HotSpot is a widely used tool to estimate processor temperature based
> on an architectural floorplan and simulated power measurements. The
> thermal simulation iteratively solves a series of differential
> equations for block. Each output cell in the computational grid
> represents the average temperature value of the corresponding area of
> the chip.

Concretely, HotSpot is a rank-1 2D stencil code. The implementation in
Futhark is similar to our [stencil
example](/examples.html#gaussian-blur-stencil), although with more
complicated edge conditions. There is a rich literature on various
clever ways to optimise stencils. Futhark does not yet employ any of
these tricks, but still manages to obtain decent performance. The main
contributor here is a memory management strategy that avoids both
copying and expensive allocations inside the outer sequential loop. We
[wrote a blog
post](/blog/2018-01-28-how-futhark-manages-gpu-memory.html) on the
technique.
