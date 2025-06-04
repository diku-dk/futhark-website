---
title: Being better at the bad
description: Futhark recently got slightly better at programs that you probably should not write in the first place.
---

While Futhark is mostly known for its GPU support, it is in fact a high level
and hardware-agnostic functional array language, where the available programming
constructs are largely unconstrained by the particularities of any specific
processor. This is unproblematic when you implement an interpreter for your
language ([as you should](2025-05-07-implement-your-language-twice.md)), but
despite its principles, Futhark *is intended* for compilation to high
performance code running on potentially restricted machines, and does [come with
many limitations](https://futhark-lang.org/blog/2016-09-03-language-design.html)
as a result. However, these restrictions are phrased in terms of syntax and type
rules, without reference to ad-hoc hardware limitations ([with a few exceptions
that we hope to address
eventually](https://futhark-lang.org/blog/2018-12-08-why-futhark-sometimes-goes-wrong.html)),
and so users are still insulated from having to worry about specific hardware
details.

Unfortunately, Futhark has to ultimately run on real hardware, and some of the
things you are allowed to do in Futhark are not directly supported by e.g. GPUs,
meaning the compiler has make up for the differences. Some of these are truly
interesting, and lead to program transformations such as [incremental
flattening](https://futhark-lang.org/blog/2019-02-18-futhark-at-ppopp.html),
which address the lack of (efficient) support for nested parallelism in
massively parallel hardware. This is a feature that is a core part of not just
Futhark as a research project, but also the vision of allowing high-level,
abstract, and modular programming of parallel computers. Writing nested parallel
Futhark programs is the natural and recommended way to write Futhark programs,
and through a principled flattening transformation, we can generate quite
efficient GPU code.

And then there are more ad-hoc technical problems. The ongoing effort to
[implement a WebGPU backend](https://github.com/diku-dk/futhark/pull/2140)
(originally by [Sebastian Paarmann](https://github.com/spaarmann), now continued
by [Caleb Andreasen](https://github.com/candrdk)) is a very fertile source of
problems, including:

1. Lack of support for double precision floating point. Fortunately we had some
   near-vestigial code for supporting GPUs that lack such support (as that was
   the case for the GPU in the laptop I had as a PhD student), which we could
   use.

2. Limited support for working with pointers as values, which prevents not just
   some [interesting memory-reuse
   optimisations](https://futhark-lang.org/blog/2022-11-03-short-circuiting.html),
   but also double buffering by pointer swapping inside GPU kernels.

3. Harsh restrictions on uniform control flow.

4. No atomics on 64-bit datatypes.

5. A somewhat more restrictive memory model than found in OpenCL or CUDA.

A lot of these restrictions are likely connected to WebGPU having to support
very weak GPUs (in the sense of not allowing general compute), but they are
quite frustrating, especially in an ostensibly modern API that is a potential
contender as a general-purpose cross-platform GPU API.

In Futhark, atomic operations are used to implement certain programming
constructs, most notably
[histograms](https://futhark-lang.org/examples/histograms.html). A Futhark
programmer can compute (generalised) histograms with any type, as long as they
provide a neutral element and an associative and commutative combining operator.
The compiler is smart enough to take advantage of hardware intrinsics when the
relevant operator is directly supported (e.g. 32-bit integer addition), but in
the general case, we fall back to either a
[CAS](https://en.wikipedia.org/wiki/Compare-and-swap) approach (when the type
question has a size matching a known CAS intrinsic - in practice 32 or 64 bits),
or in the worst case, a mutex-based scheme based on spinlocks. ([We wrote a
paper about this.](https://futhark-lang.org/publications/sc20.pdf))

Due to point (5) above, the spinlocks don't work on WebGPU. For operators that
truly need mutexes, this is an unavoidable problem, but for operators that work
on values *smaller* than 32 bit (e.g. 16 bit integers), it is actually possible
to implement these via 32 bit atomics, rather than via spinlocks. The trick is
to fetch whichever 32-bit word your desired 16-bit halfword belongs to, update
the pertinent part via the usual bitmasking and shifting tricks, then atomically
replace the word in memory with a CAS operation. This can of course fail if
someone else has modified the 32-bit word since you fetched it, in which case
you simply try again.

We have [implemented this in the Futhark
compiler](https://github.com/diku-dk/futhark/pull/2262), and it does show some
speedups. On a [microbenchmark for histograms on 8-bit
integers](https://github.com/diku-dk/futhark-benchmarks/blob/13d3cb5cb2c887adca2bf4fbd02f9e866436cbfe/micro/reduce_by_index.fut#L101-L127)
where we vary the number of histogram buckets but keep the number of updates
fixed, the spinlock approach obtains these performance numbers on an A100 GPU,
measured with [futhark
bench](https://futhark.readthedocs.io/en/latest/man/futhark-bench.html):

```
reduce_by_index.fut:sum_i8 (no tuning file):
10i32 [1000000]i32 [1000000]i8:                  111μs (95% CI: [     111.4,      111.5])
100i32 [1000000]i32 [1000000]i8:                 112μs (95% CI: [     111.8,      111.9])
1000i32 [1000000]i32 [1000000]i8:                122μs (95% CI: [     122.0,      122.1])
10000i32 [1000000]i32 [1000000]i8:               203μs (95% CI: [     202.3,      204.2])
100000i32 [1000000]i32 [1000000]i8:              202μs (95% CI: [     202.2,      202.6])
```

And now with the CAS-based implementation we get these:

```
reduce_by_index.fut:sum_i8 (no tuning file):
10i32 [1000000]i32 [1000000]i8:                  117μs (95% CI: [     117.2,      117.4])
100i32 [1000000]i32 [1000000]i8:                 115μs (95% CI: [     114.4,      114.8])
1000i32 [1000000]i32 [1000000]i8:                127μs (95% CI: [     126.7,      126.8])
10000i32 [1000000]i32 [1000000]i8:               166μs (95% CI: [     166.2,      166.7])
100000i32 [1000000]i32 [1000000]i8:              162μs (95% CI: [     161.5,      161.7])
```

The impact here is not great - a 1.25x speedup in the best case. The updates are
uniformly distributed in the above; with a maliciously crafted input that has
more conflicts we would see the CAS-approach perform significantly better than
spinlocks. However, the performance of this concrete case is not really the
point (since we would have made this change for the sake of WebGPU anyway), but
to which extent Futhark should put in an effort to speed up code that should
probably not be written this way. If you want to compute a histogram, then it is
*certainly better* to use a directly supported 32-bit or 64-bit type, rather
than rely on simulation. This much better, in fact:

```
reduce_by_index.fut:sum_i32 (no tuning file):
10i32 [1000000]i32 [1000000]i32:                  93μs (95% CI: [      92.9,       93.5])
100i32 [1000000]i32 [1000000]i32:                 91μs (95% CI: [      91.4,       91.5])
1000i32 [1000000]i32 [1000000]i32:               103μs (95% CI: [     102.6,      102.8])
10000i32 [1000000]i32 [1000000]i32:              178μs (95% CI: [     177.4,      177.8])
100000i32 [1000000]i32 [1000000]i32:              68μs (95% CI: [      68.3,       68.5])
```

I think it's a reasonable question. However, I do enjoy the dream that Futhark
will *just work*, no matter what hardware you have, although we're still some
way away from that (and [Futhark will not do everything for
you](https://futhark-lang.org/blog/2022-04-04-futhark-is-a-low-level-language.html)).
There are also some cases where the need for weird atomics arise due to code
that is written in what I would consider "good style"; in particular they can
arise from [reverse-mode automatic
differentiation](https://futhark-lang.org/examples/reverse-ad.html) of functions
that perform scalar 8-bit and 16-bit arithmetic for sensible reasons.
