---
title: Faster OpenCL Float Atomics
description: Friendly folks on the Internet published some code we can use.
---

I recently wrote [a blog post on performance differences between
OpenCL, CUDA, and HIP](2024-07-17-opencl-cuda-hip.html). Many of these
differences are because OpenCL does not expose all the functionality
of the underlying hardware. As one particular example, OpenCL does not
expose any functionality for performing atomic floating-point
operations. This is a problem, as atomics are used for implementing
Futhark's [generalised
histograms](https://futhark-lang.org/blog/2018-09-21-futhark-0.7.1-released.html#histogram-computations),
which are a useful building block for some irregular applications.
While floating-point atomics can be simulated through judicious use of
[compare-and-swap](https://en.wikipedia.org/wiki/Compare-and-swap),
this is nowhere as efficient as proper hardware-supported atomics when
there are many conflicts.

To see the impact, consider this benchmark program:

```Futhark
entry main32 [m][n] (hist : *[n]f32) (is: [m]i32) (image : [m]f32) : [n]f32 =
  reduce_by_index hist (+) 0f32 (map (%100) (map i64.i32 is)) image

-- ==
-- entry: main32
-- random input { [100000]f32 [10000000]i32 [10000000]f32 }
```

We are computing a generalised histogram with 100,000 bins, but where
the 10,000,000 samples all fall within the first 100 bins, meaning we
will have lots of conflicts in the updates. Futhark employs a rather
sophisticated algorithm to minimise conflicts, [documented in this
paper](https://futhark-lang.org/publications/sc20.pdf), but it is not
perfect, and is particularly bad for histograms that are both very
large and very sparse. Using the CUDA backend, which supports atomic
floating-point operations natively, we obtain (on an NVIDIA A100):

```
$ futhark bench --backend=cuda hist.fut
Compiling hist.fut...
Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
More runs automatically performed for up to 300s to ensure accurate measurement.

hist.fut:main32 (no tuning file):
[100000]f32 [10000000]i32 [10000000]f32:       4630μs (95% CI: [    4625.5,     4642.0])
```

About 4.6ms. Decent runtime, depending on your expectations. Now let
us try with the OpenCL backend, which uses a CAS-loop:

```
$ futhark bench --backend=opencl hist.fut
Compiling hist.fut...
Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
More runs automatically performed for up to 300s to ensure accurate measurement.

hist.fut:main32 (no tuning file):
[100000]f32 [10000000]i32 [10000000]f32:     105876μs (95% CI: [  101517.4,   118794.8])
```

106ms! That's over 20x slower. Not great. However, about a week ago
[PipInSpace wrote a blog post about a more efficient way of atomically
adding floating-point
numbers](https://pipinspace.github.io/blog/atomic-float-addition-in-opencl.html),
which is largely based around detecting vendor-specific OpenCL
extensions, and falling back if necessary to a somewhat cleverer
CAS-loop than Futhark was using. Today I spent a few hours
[implementing this technique in the Futhark
compiler](https://github.com/diku-dk/futhark/pull/2181), including
extending the approach to also handle double-precision numbers, and
now our OpenCL performance looks much better:

```
$ futhark bench --backend=opencl hist.fut
Compiling hist.fut...
Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
More runs automatically performed for up to 300s to ensure accurate measurement.

hist.fut:main32 (no tuning file):
[100000]f32 [10000000]i32 [10000000]f32:       5534μs (95% CI: [    5521.7,     5539.2])
```

It is still not *quite* at the level of CUDA, but I think this is not
due to the atomics, but rather due to OpenCL not providing [precise
hardware
information](https://futhark-lang.org/blog/2024-07-17-opencl-cuda-hip.html#cause-imprecise-thread-information)
to the Futhark runtime system. I am looking forward to someone else
writing a blog post about how to also address that problem, so I don't
have to do the work myself.
