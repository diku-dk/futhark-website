---
title: Nondeterminism in Futhark
description: A description of which parts of Futhark are technically nondeterministic in some cases, what nondeterminism means in this case, and in which cases perfect determinism can be guaranteed.
---

Last week I attended [IFL2026](https://ifl26.cse.chalmers.se/), which was
probably one of the best conference experiences I have ever had. One of the many
good presentations was by [Michael Haddon](https://michael.haddon.me/), who
presented [The Replay
Lattice](https://michael.haddon.me/research/replay-lattice), which is a type
system for floating-point reproducibility. The type system was presented for a
demonstration language called Lane, but it inspired me to think more carefully
about nondeterminism in Futhark (there is some), and what it might cost to get
rid of it.

I often use the terms "determinism" or "deterministic" when explaining why I
think data parallel programming is such a good paradigm, so perhaps it is
surprising to see me write a blog post about nondeterminism in Futhark. But as
is so often the case with such bold promises, they often tend to get a little
weaker once you look closely enough. Michael Haddon's work defines some
nomenclature that is helpful for this discussion:

* **Exact** means that the computation produces the same result every time,
  guaranteed. (Michael describes this as matching the "serial reference", which
  I think is not the best way of looking at it, but that's a question of
  perspective)

* **Replay** means that if you rerun the computation with the same "schedule"
  (in some sense "machine"), you will get the same result, but other machines
  may give different results.

* **Approx** means potentially nondeterministic results.

The kind of nondeterminism we are talking about here is not the kind of
free-form nondeterminism that arises from classic shared-state concurrent
programming. Indeed, both Lane and Futhark are pretty much exclusively concerned
with the kind of nondeterminism that arises from floating-point rounding errors.
The simplest example is the non-associativity of addition:

```
> (0.1 + 0.2) + 0.3 == 0.1 + (0.2 + 0.3)
false
```

If we have an array of numbers we need to sum, then the result depends on the
order in which you sum them. When parallelising a summation, you often do things
like split the numbers into blocks, sum each block on a separate processor, then
combine the partial results. This changes the order of addition, and hence the
result. Now, an important point is that there is no reason to think that the
parallelised result is numerically *worse* than a serial left-to-right
summation, but it will be different. Further, an efficient implementation will
likely choose a number of chunks somewhat proportional to the number of
processors on the machine, which means the result may also be different on other
machines (but again, *not necessarily worse*). This corresponds to the "Replay"
class of nondeterminism. Finally, the partial results may be combined using
[atomic instructions](2024-09-23-faster-opencl-float-atomics.html), which may be
resolved in a different order when the program is rerun on the same machine.
This is then "Approx".

Even in this simple example of summing a bunch of numbers, we encounter both
classes of nondeterminism. Note that none of them are "race conditions" in the
usual sense of the program sometimes just *malfunctioning*: the point is that
the result changes, but given that the result is in some sense already "wrong"
(due to roundoff error), the main issue is that there are cases where we care
about getting the *same* "wrong" result every time.

So in Futhark, ostensibly a deterministic language, where does the
nondeterminism come from? There are a few sources. Some of them come when
programmers violate mandated constraints (but which are unchecked for various
reasons), and others are basically down to floating-point roundoff error.

## Language abuse

Let us start by considering language abuse. The worst but also least interesting
case is using the `#[unsafe]` [attribute](2020-06-28-attributes-in-futhark.html)
to disable safety checks, after which you can do arbitrary memory reads and
[writes](2022-06-13-uniqueness-types.html). This can be avoided by passing
`--safe` to the compiler, or perhaps by just not doing this kind of cowboy
coding in the first place. If you are a good enough programmer that your code
does not need any safety checks, why not just go and write in C? Those folks
never make mistakes.

Another more insidious problem is related to the `scatter` construct. The
semantics of an expression `scatter d is vs` are simple enough, and can be
expressed with this imperative pseudocode:

```
for (i,v) in zip(is, vs):
  res[i] = v
```

That is, `scatter d is vs` pairs the indexes in `is` with the corresponding
values in `vs` and writes each value to the corresponding index. It is a
parallel random-access write. To ensure that the writes can indeed be written in
parallel, Futhark requires that `is` contains no duplicates, or in a slight
weakening, that if `is` contains duplicates, then the corresponding values are
also identical. This is *not* checked dynamically, and a program can get in an
awful lot of trouble by violating this property. Which write "wins" is
unpredictable - in the worst case, you can even get a "torn" write that combines
two different values.

This behaviour of `scatter` is thorny, and even experienced Futhark programmers
sometimes make this mistake. The reason it has no safety checks is twofold:

1. `scatter` is a [fundamental
   construct](2019-04-10-what-is-the-minimal-basis-for-futhark.html) used in
   most sophisticated parallel algorithms, and so its performance is paramount.

2. I'm not sure how to detect duplicate writes in a way that does not violate
   the [cost model](2022-01-27-cost-models-are-contracts.html) (although
   [`scatter` is already a bit iffy in that
   regard](2026-02-04-when-cost-models-hit-the-wall-clock.html)). Basically, the
   cost of `scatter` must be proportional to the number of elements written, not
   to the total size of the target array, and that makes it difficult to
   construct appropriate auxiliary structures for safety checks.

And yes, I do realise the irony of insisting on an unsafe `scatter` after making
fun of C programmers for using an unsafe language.

Anyway, let's move on to `reduce`. Parallel reductions, just like parallel
scans, histograms, and so on, work only if the provided operator is
[associative](https://en.wikipedia.org/wiki/Operator_associativity) and in some
cases also commutative. This is documented but unchecked - in Futhark you can
do a `reduce` with an arbitrary operator of the right type, and it just might
give you nonsensical or nondeterministic results. It is a common error among new
Futhark programmers to treat `reduce` as a general fold and use operators that
are not associative, but it seems not to show up so much among more experienced
programmers. The reason we do not check for associativity is that [it is
impossible](https://en.wikipedia.org/wiki/Rice%27s_theorem), and even when it is
possible it remains hard - well beyond what a practical type checker is capable
of. It is not too hard to [empirically test
associativity](../examples/testing-associativity.html) and other properties, and so it seems
that this source of nondeterminism is not so problematic in practice.

That actually concludes the list of "true" nondeterminism in Futhark, and in
both cases we can wash our hands and blame the programmer. Of course, there is
one rather important caveat here...

## The floating-point problem

Strictly speaking, floating-point addition is not associative, and so we could
just declare that `reduce (+) 0` on floating-point numbers is simply wrong.
However, Futhark is not mainly intended as a framework for explaining why the
program you wrote is wrong, but rather as a practical tool for parallel
programming, and clearly people *want* to sum floating-point numbers. Indeed,
summing and multiplying floating-point numbers is worth trillions of currency
units these days!

As such, Futhark's `reduce` (and similar functions) do not say that the result
you get back is completely arbitrary in case the provided operator is
nonassociative. Instead, they say that the result you get back is some
*arbitrary ordering* of applying the operator to the input elements. The precise
ordering is unspecified (we'll return to this), but it will be *some order*, and
if the function is associative, it just so happens that the order *does not
matter for the final result*.

The application order chosen by Futhark depends on the backend and the hardware.
With the `c` backend, it will be a straightforward left-to-right loop across the
input array. With the GPU backend, it will split the input into chunks, sum them
separately, and combine the partial results. The number of chunks depends on
runtime queries of the GPU hardware topology, and can also be configured by the
user via [tuning parameters](2026-02-03-tuning-params.html). Since the chunking
strategy (the "schedule") depends on the hardware and the input size, repeatedly
re-running the same reduction on the same system will consistently give the same
result - the "Replay" case in Michael Haddon's terminology. This is not a
language guarantee, but it is how it is implemented right now.

A reasonable question is whether we could implement summation in a different way
that will *always* give the same result, no matter the input. Indeed, this is
possible. Consider for simplicity that the input array is a power of two. We can
then implement reduction by repeatedly splitting the array in half, combining
the two halves using the provided operator, and continuing until only a single
element is left:

```
def sum_tree (as: []f64): f64 =
  let as' = loop as while length as > 1 do
              let m = length as / 2
              in map2 (+) (take m as)
                          (take m (drop m as))

  in as'[0]
```

We can generalise this to arbitrary operators (even non-commutative ones) and
non-power-of-two sizes with a bit more complexity. Note that this implementation
does not use any Futhark constructs that are nondeterministic, hence *by
construction* it is also fully deterministic. Will it produce the same result as
a sequential `foldl (+) 0`? No, because the order of application is different,
but if you compile `sum_tree` with the `c` backend then it will produce
*exactly* the same results as if you compile with the `cuda` backend. This is
the "Exact" case.

There is a reason we don't implement `reduce` like that, of course. In a quick
experiment for summing 100 million numbers, I observe that using `reduce (+) 0`
is almost *3x* faster than `sum_tree` on an MI100 GPU. The gap can be narrowed
by using blocking to reduce the number of GPU kernels necessary, but ultimately
the fastest way to perform a reduction does depend on fine details of the
hardware, and when your operator is not truly associative, that will result in
observable differences.

The same situation arises with Futhark's other basic building blocks. I suspect
`reduce` is actually the *best* case, in the sense that a hand-written version
can approach the performance of the built-in one. For `scan`, I know that the
compiler's fancy [decoupled
lookback](https://research.nvidia.com/sites/default/files/pubs/2016-03_Single-pass-Parallel-Prefix/nvr-2016-002.pdf)
is much faster than anything that can be written in Futhark itself, and for
`reduce_by_index` a fully deterministic implementation involves a [sort and a
segmented
reduction](2018-09-21-futhark-0.7.1-released.html#histogram-computations), which
is *wildly* slower than the code produced by the compiler, which makes careful
use of atomic instructions.

This does make me wonder if one could write a Futhark library that replaces all
the SOAC implementations with guaranteed deterministic implementations, and what
the performance of that might be across the [benchmark
suite](https://github.com/diku-dk/futhark-benchmarks), but that will have to
wait for a future post.
