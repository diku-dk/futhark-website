---
title: Supporting half-precision floats is really annoying
description: I just added support for half-precision floats to Futhark, and it was much more annoying than expected.
---

I just added half-precision floats to Futhark, as the type `f16`.
This wasn't particularly hard (perhaps a day of work), but it was
*annoying* for fairly shallow technical reasons, so as a bit of
catharsis, here's a blog post about the challenges I encountered.

## Why

First of all, why do we even *want* 16-bit floats?  Their precision is
horrible and the largest representable value is a mere 65504.
Apparently 16-bit floats were used by various exotic processors in the
early 80s, but the modern lineage, as eventually standardised by IEEE
754-2008, hails from graphics programming.  This is because
half-precision floats can be useful for expressing light intensities,
as they are *accurate enough*, while allowing much larger dynamic
range than a 16-bit fixed point number.  Initially, half-precision was
only used as a storage format, but eventually GPUs grew support for
operating directly on half-precision floats; often with twice the
arithmetic throughput.  Since they only take up half as much space in
memory as single-precision floats (and hence also use half the memory
bandwidth), they are in theory "twice as fast" as single-precision
floats.

Half-precision floats have also become increasingly popular for use in
machine learning applications, as it appears neural networks are
resistant to numerical problems (presumably they just train around
them).  But this is where things get interesting: there are actually
(at least) *two* half-precision float formats.  All take up 16 bits in
memory, but differ in how they allocate those bits to significand and
exponent.  The one we support in Futhark is [the *binary16* format
from IEEE
754-2008](https://en.wikipedia.org/wiki/Half-precision_floating-point_format#ARM_alternative_half-precision),
but Google has also developed the
[bfloat16](https://en.wikipedia.org/wiki/Bfloat16_floating-point_format)
for use in AI accelerators.  ARM apparently also supports a
nonstandard variation of binary16, but I'm hoping I will never have to
learn the details.

So we want to support half-precision floats because some applications
can make use of them, either because they take up less space or
because computation on them is actually faster.

## How

Half-precision floats are standardised in IEEE 754-2008, but they are
*not* a universally supported a type in common programming languages.
In particular, ISO C does not support them.  Other languages have
unusual restrictions where `half` is supported as a "storage type"
(meaning you can put them in memory), but you need to convert them to
some other type (e.g. `float`) if you actually want to do arithmetic
on them.  This is not how we want to do it in Futhark.  The `f16` type
should be fully supported like any other primitive type, and on all
systems.  When necessary, the compiler must generate code to paper
over lack of hardware support.

### C

Most of Futhark's backends generate C code - either plain CPU C,
OpenCL C, or CUDA C.  Let's start with plain C, which does not support
a `half` type.  One option would be representing `f16` as a 16-bit
integer and then implementing every single arithmetic operation
manually.  [People have done so](http://half.sourceforge.net/), but
such code runs extremely slowly.  Instead, we simply `typedef float
f16` and actually perform the `f16` operations in single precision.
However, we don't want these simulated floats to take up 32 bits per
element in memory, so we distinguish between the *scalar type* used
for operations, which is `float`, and the *storage type* used for
arrays, which is `uint16_t`.  When reading an `f16` from an array, we
must use a function to convert from the storage type to the scalar
type - fortunately, the library linked above contained such a function
and its inverse.  This means that `f16` operations might produce
different results depending on whether they are emulated or not, as
emulation only performs the rounding at the end, while true `f16`
calculations might also round intermediate results.  I'm not
sufficiently experienced with numerical analysis, or the guarantees of
IEEE 754, to say whether this is a real problem.

We also have a problem with [Futhark's C
API](https://futhark-lang.org/blog/2017-09-26-calling-futhark-from-c-and-haskell.html).
Normally an entry point that returns a scalar will be exposed as a C
function that produces a value of the obvious type (`int32_t` for
`i32` and so on), but C does not have a `half` type.  Again we fall
back to the notion of "storage type" and return `f16` results as
`uint16_t`.  Life would be so much easier if `half` was universally
supported.

### OpenCL

Let's move on to the OpenCL backend.  Surely life is easier there!
No, it never is.  OpenCL *does* actually define a `half` type, but
only as a storage format, meaning you can have pointers to them, but
not perform any arithmetic, unless the platform in question supports
the
[`cl_khr_fp16`](https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/cl_khr_fp16.html)
extension (and you enable it).  OpenCL platforms are loaded
dynamically followed by run-time compilation at kernels, so the
Futhark compiler does not know at compile-time whether the target platform
supports half-precision.  Therefore we generate [kernel code with
plenty of
#ifdefs](https://github.com/diku-dk/futhark/blob/23956cc4e432e1cfbff9d3908fd8863dfaed6640/rts/c/scalar_f16.h#L11-L34)
to detect whether the platform supports `cl_khr_fp16`, and fall back
to `float`-based emulation otherwise.  Remarkably, OpenCL *does*
provide builtin functions for [efficiently translating between
single-precision and half-precision floats stored in
memory](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/vload_half.html),
even for those platforms that don't have `cl_khr_fp16`.  This lets us
load half-precision floats into single-precision scalars at quite high
speed.  I noticed about an order of magnitude difference compared to
the hand-written conversion function.  This is quite important, since
for some reason, NVIDIA's OpenCL implementation does *not* support
`cl_khr_fp16`, despite their GPUs being fully capable.

As a delightful quirk, the OpenCL emulator
[Oclgrind](https://github.com/jrprice/Oclgrind) claims to support
`cl_khr_fp16`, but will fail to actually perform any operations on
`half` values.  Futhark users will never use Oclgrind, but it is an
absolutely indispensable tool for hacking on the compiler, and is used
in our regression test suite as well.  The solution is an [ad-hoc
check to always emulate half-precision on
Oclgrind](https://github.com/diku-dk/futhark/blob/master/rts/c/opencl.h#L739-L743).

### CUDA

Okay, time for CUDA.  NVIDIA has been marketing half-precision hard,
so *surely* everything will just work out, right?

Sure.

So, CUDA does indeed support half-precision floats on devices that are
Compute Capability 6.0 or newer.  This can be checked with an
`#ifdef`.  However, for some strange reason, you have to include a
special header file, `cuda_fp16.h`, to actually get access to the
`half` type and its operations.  Glancing at the header file, it looks
like `half` is actually implemented as a C++ class with inline PTX
(CUDA assembly) to perform the computations using the natively
supported half-precision instructions.  This is quite baroque.  Why
isn't the `half` type just implemented in the compiler, like it is in
OpenCL?

I *suspect* the reason is that CUDA is mostly a "single source"
programming model where the same program contains both CPU and GPU
code, where the CPU code is ultimately compiled with the system
compiler - which does not support half-precision floats.  In contrast,
OpenCL C is specifically for "device" code (read: GPU code), and the
"host" (CPU) is completely out of its purview.  Futhark does not use
CUDA in a "single source" manner, but instead uses
[NVRTC](https://docs.nvidia.com/cuda/nvrtc/index.html) to do run-time
compilation of CUDA kernels, very similar to how OpenCL works.

The need to include `cuda_fp16.h` is not just a compiler
implementation curiosity - it turns out to present the *greatest*
annoyance in this entire cavalcade of accidental complexity.
Apparently when you run-time compile a CUDA kernel with NVRTC, the
default search path does not include the CUDA include directory, so
*this header cannot be found*.  Whoever calls the NVRTC API must
explicitly provide the directory containing this header.  This is
*bad*.  I've long sought to ensure Futhark didn't [contribute to the
growing accidental complexity
disaster](https://gist.github.com/cookrn/4015437), with new
configuration files and environment variables being my main aversion.

But now it seems we have no choice.  Futhark *must* be able to figure
out, at run-time, where CUDA is installed, and there is no standard
for this.  Sure, it's often in `/usr/local/cuda`, but it's fairly
common to put it somewhere in `/opt` as well, especially on
supercomputer systems.  And there is *no standard environment
variable* pointing at the CUDA directory!  Other GPU applications
respect any or all or `CUDA_HOME`, `CUDA_PATH`, and `CUDA_ROOT`.  And,
to my great regret, [now Futhark does as
well](https://github.com/diku-dk/futhark/blob/23956cc4e432e1cfbff9d3908fd8863dfaed6640/rts/c/cuda.h#L348-L358).

This is a tragedy and a shame.  There is no good reason that is has to
be this way.  I *know* that this will result in things not working
down the line, just because a user didn't set an invisible global
variable correctly.  I still hope that I just missed a subtle detail
somewhere, and that this will eventually go away like a bad dream.

## Gotta go fast?

Half-precision floats are the worst IEEE floats in all ways except
speed.  So are they faster in Futhark?  Sort of.  On an A100 GPU, the
expression `f16.sum` can sum one billion half-precision floats in
*2.1ms*, while `f32.sum` takes *2.9ms* to sum one billion
single-precision floats.  That's a speedup of *1.38x*, which is much
less than the *2x* we would theoretically expect.  So what's going
wrong?  Two things:

1. Adding two half-precision floats on their own is probably not
   faster than adding two single-precision floats.  [CUDA's
   half-precision
   API](https://docs.nvidia.com/cuda/cuda-math-api/group__CUDA__MATH__INTRINSIC__HALF.html#group__CUDA__MATH__INTRINSIC__HALF)
   strongly implies one should consider using [the `half2`
   type](https://docs.nvidia.com/cuda/cuda-math-api/group__CUDA__MATH____HALF2__ARITHMETIC.html#group__CUDA__MATH____HALF2__ARITHMETIC),
   which packs two half-precision numbers in 32 bits, and allows
   "vectorised" operations on both in a single instruction.  As far as
   I understand current hardware, this is the way half-precision
   floats can achieve high arithmetic throughput.  I don't know if the
   CUDA compiler will optimise multiple `half`s into `half2`s on its
   own, but I doubt it - especially for a reduction.

2. The analysis above is valid, but it's not actually what is
   happening here.  This summation is *completely* bandwidth-bound, so
   the only thing that matters is how efficiently we access memory.
   In `f16.sum`, each thread reads a 16-bit value at a time.  While
   the Futhark compiler arranges things so that the accesses are fully
   [coalesced](https://cvw.cac.cornell.edu/gpu/coalesced), the GPU's
   memory architecture is not really designed for individual threads
   issuing such small reads - perhaps each thread will actually read
   (at least) 32 bits, then throw half of it away.  In any case, the
   result is that the memory bus is not fully utilised.

A more efficient half-precision reduction would have each thread read
multiple packed values with a single transaction, such as by reading a
32-bit `half2`.  This is a trick we'll have to teach the compiler at
some point.  This is also not a new concern - if you write a program
that sums 8-bit or 16-bit integers, you are also not fully utilising
the memory bus, even if in absolute runtime, it's still faster than
summing the same number of 32-bit integers.

How do we verify that this analysis is correct?  We can write a
program that interprets an array of `u32` values as packed `f16`s,
using a `map`-`reduce` composition:

```Futhark
map (\x -> f16.from_bits(u16.u32(x>>16)) +
           f16.from_bits(u16.u32(x)))
>-> f16.sum
```

This can sum the equivalent of a billion `f16`s (that is, half a
billion `u32`s) in a mere *1.5ms* - a *1.93x* speedup over `f32`
summation.

## So should you use single-precision floats?

Unless you know what you are doing, you probably should not use `f16`
in your code.  Their numerical properties are absolutely going to bite
you.  Interoperability is awkward and the performance isn't even that
great *yet*.  But from a language perspective they are now fully
supported, and hopefully we'll make them run even faster in a not too
distant future.  But I doubt we'll add more bits, so the numerics are
always going to be dubious.
