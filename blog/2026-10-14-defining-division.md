---
title: Making all divisions slower to fix a bug in 0.000000000000000000542% of cases
description: The C code produced by the Futhark compiler occasionally exhibits undefined behaviour, and this post is about how we fixed one of those cases, and what it costs us.
---

Almost all modern machines use [two's
complement](https://www.cs.cornell.edu/~tomf/notes/cps104/twoscomp.html) to
represent integers. This representation is asymmetric, in the sense that the
most negative number has no corresponding positive number. For example, with
signed 8-bit numbers, the most negative number is `-128`, whilst the most
positive number is `127`. This quirk shows up in various cases, but for the
purpose of this post, I am concerned with signed integer division, where `x/-1`
mathematically should produce `-x`, but in the case where `x` is the most
negative number, that is not possible. For 32-bit integers, which I arbitrarily
decide are the most common, this means that one out of every 2⁶⁴ possible inputs
is impossible to represent correctly - leading to the somewhat clickbaity title
of this post, and some recently fixed troubles in the Futhark compiler. In this
post I will explain what the actual problem is for a language implementation,
how we fix it, and what the performance cost looks like. The main audience of
this post is other language implementers, but I think the concepts involved are
simple enough that anyone curious about low level issues can follow along.

## The C issue

The Futhark compiler has multiple backends, but the most important ones work by
generating C code. This has many practical advantages:

1. It is easy to debug the generated code because it is easy to read and modify
   by hand.

2. Most systems have a C compiler (or easy access to one), meaning Futhark has
   fewer dependencies, is portable, and doesn't need to bundle native code
   generators.

3. Many languages can interoperate with C, so by exposing Futhark as a C API,
   [those languages can also call
   Futhark](2022-07-01-how-futhark-talks-to-its-friends.html).

That said, there are also disadvantages:

1. Some code patterns are hard to express (or express efficiently) in C. This is
   mainly the case for advanced language features such as coroutines, tail
   calls, or garbage collection. Fortunately Futhark is [pretty boring from a
   semantic perspective](2025-09-26-the-biggest-semantic-mess.html), so this
   does not affect us too much.

2. It is very easy to accidentally generate C code that is subtly wrong. This
   does unfortunately keep pestering us.

C is of course renowned as a very safe and easy-to-use language, because C
programmers never make coding mistakes. The few times they do make mistakes, it
is often due to mishandling of pointers or array sizes, leading to buffer
overflows, heap smashing, etc. These are actually not the main problem with
generating C from Futhark, as Futhark itself is memory-safe, and systematically
inserting in bounds checks in generated code is easy ([although a little more
interesting in GPU code](2020-07-13-bounds-checking.html)).

No, the main problem we've had is actually undefined behaviour. As is by now
well-known, C specifies various events as explicitly undefined, meaning not only
that a C compiler can assume it will never occur, but also that no particular
result is guaranteed should a program perform one of the undefined operations.
Some causes of undefined behaviour are obvious and easy to avoid: reading arrays
out-of-bounds, using freed memory, etc. But C has a long list of more obscure
cases. My favourite example is that in C99, it is undefined behaviour if a
source code file does not end with a final newline character. Yet we have
honestly not spent that much time dealing with that particular issue in Futhark.

No, the main problem, and now we are finally returning to the premise in the
post, is that *signed* integer overflow is undefined behaviour in C. This is
particularly insidious since most programmers have a mental model of fixed-size
integer arithmetic where overflow is a natural event, and so does the actual
machines we use, but a C compiler may assume it never happens. The reason behind
this specification is so the C compiler can optimise based on useful properties
such as `x+1>x`, which are not true in the presence of overflow, but the
motivation does not actually matter to us. In a language like Futhark, where
integer overflow *is* defined behaviour (it wraps around), this creates a very
unfortunate mismatch between what we want to do, and what we can express in our
target language.

Fortunately, C is merciful enough to specify that for *unsigned* numbers,
overflow is well-defined, and behaves just as you would expect (on a
two's-complement machine, anyway). For this reason, Futhark's C code generation
actually represents all integer types using unsigned C types, (so `i64` becomes
`uint64_t`), and then performs the arithmetic on unsigned types. This works due
to the nice property of two's complement representation that most arithmetic
operations are done exactly the same, at the bit level, as for unsigned
integers. One exception is comparison operators, in which we cast to the signed
type (e.g. `int64_t`) before performing the comparison. Such conversions are
free, since they do not need to change the bit representation of the number, but
only affect what instructions the C compiler emits for e.g. a `<=` operation.

Another operator where sign matters is division. If we do `x/-1`, then it is
quite important whether we are dividing with a small negative number, or with
the number that arises if we interpret the two's complement bit pattern for `-1`
as an unsigned integer. This means that for signed integer division, Futhark
generates a signed C division. This means that if we do `x/-1` where `x` is the
most negative number, we are stuck with the issue described in the post opener -
the correct result (`-x`) cannot be represented as a signed integer, so this is
a signed integer overflow, and that is undefined behaviour in C.

Futhark is a principled language, and therefore producing undefined behaviour is
unacceptable. (I realise the hypocrisy of this puritan declaration whilst
leaving in the bug for ten years.) Further, on x86 this kind of division will
actually signal a CPU exception, crashing the program, so this is not merely a
violation of the sanctity of the C specification. Fixing it in principle is
easy. This is the definition of 32-bit signed division in the Futhark runtime
system:

```C
int32_t squot32(int32_t x, int32_t y) {
  return x / y;
}
```

And this is the version that avoids the 0.000000000000000000542% undefined cases:

```C
int32_t squot32(int32_t x, int32_t y) {
  return y == -1 ? (int32_t)(0 - (uint32_t)x) : x / y;
}
```

This function simply checks whether the divisor is `-1`, and if so does the
negation with unsigned (well-defined) arithmetic. The cost is a branch on every
single division. The behaviour is still *implementation-defined*, but produces
the right results on the machines we care about.

(Why `quot` instead of `div`? Futhark actually has *two* division operators -
one that rounds sensibly towards negative infinity, and one that rounds towards
zero like C. The function above is the one that follows C's behaviour because it
is less code, but the issue is the same either way.)

## The cost

Correctness is not really a thing we can compromise on, but in principle it is
frustrating that *every* integer division needs an additional branch just to
handle a *single* special case that may never actually happen, as it is highly
unlikely that a program's correctness *depends* on wraparound for signed integer
division at the ends of the numeric scale. But apart from the principal
frustration, what is the actual overhead?

Measured on the [Futhark benchmark
suite](https://github.com/diku-dk/futhark-benchmarks) on GPU, I was unable to
measure an impact. This is likely because signed integer divisions are
relatively rare, and even when they do occur, they likely occur due to index
arithmetic, where the resulting memory loads have so much latency that they
easily [hide](https://modal.com/gpu-glossary/perf/latency-hiding) the cost of
doing the divisions.

But what if we try to construct a contrived program where the bottleneck is
signed division throughput? That would demonstrate something close to worst-case
overhead of adding the branch. Behold, `divisions.fut`:

```Futhark
-- ==
-- random input { 1000000 [1000]i32 [1000]i32 }

entry main [n] (k: i32) (xs: [n]i32) (ys: [n]i32) : [n]i32 =
  map2 (\x y ->
          loop z = x
          for _i < k do
            #[unsafe]
            z // y)
       xs
       ys
```

The program accepts a number `k` and two arrays of integers. For each
corresponding pair `x` and `y`, it `k` times divides `x` by `y`, using `//` to
target the `squot32` function, and using `#[unsafe]` to remove division-by-zero
checks.

For most values of `k`, repeated division quickly produces zero, but the
compiler cannot assume this, so it will have to actually perform the divisions.
You may reasonably argue that the program is not *purely* division bound, as the
bookkeeping around the loop also involves costly things such as adding and
comparing integers, but a program that contains a very long *unrolled* sequence
of divisions stretches my credulity too far.

Using the handy [futhark
bench](https://futhark-lang.org/examples/benchmarking.html) tool, we can then
measure the impact using the compiler with a fixed implementation of `squot32`
([part of this pull request](https://github.com/diku-dk/futhark/pull/2579)).
First on sequential code, using the `c` backend:

```
$ futhark-old-and-busted bench divisions.fut
...

divisions.fut (no tuning file):
100000i32 [10000]i32 [10000]i32:    3390148μs (95% CI: [ 3387613.7,  3396498.1])

$ futhark-new-hotness bench divisions.fut
...

divisions.fut (no tuning file):
100000i32 [10000]i32 [10000]i32:    3389215μs (95% CI: [ 3387626.2,  3391036.4])
```

There is no measurable difference, which is not a surprise on a modern
speculatively executing CPU for a program with an easily predicted branch.

But what about GPUs? After all, "GPUs are bad at branching" is a common motto by
those of my students who like to replace an `if` with ten lines of dense
branchfree code. Let's try the experiment again with Futhark's `cuda` backend:

```
$ futhark-old-and-busted bench --backend=cuda divisions.fut
...

divisions.fut (no tuning file):
100000i32 [10000]i32 [10000]i32:       5548μs (95% CI: [    5546.4,     5552.2])

$ futhark-new-hotness bench --backend=cuda divisions.fut
...

divisions.fut (no tuning file):
100000i32 [10000]i32 [10000]i32:      12592μs (95% CI: [   12563.5,    12622.1])
```

Well then! This really is a major difference. Much more than I expected when I
began this work, actually. Reading the generated PTX (CUDA machine code), the
old version is partially unrolled into four consecutive `div.s32` instructions
in a single basic block, whilst the fixed compiler still triggers unrolling, but
with many branches between the divisions.

I observed a similar result on an AMD GPU with the `hip` backend, although the
impact was slightly less there.

## Conclusions

The cost of fixing those 0.000000000000000000542% of divisions is real and
unfortunate, and I would certainly like to have a cheaper fix. Having undefined
behaviour in the generated code is of course not acceptable, so we will have to
live with this for now. On non-contrived benchmarks, it seems there is
essentially no impact. For programmers unwilling to pay this cost, you can
always stick to *unsigned* divisions, if you know that you will never have
negative numbers.

This is the last instance of undefined behaviour in Futhark's generated C code
that I remember. There is however at least one instance of
*implementation-defined* behaviour, namely in the code generation for
bit-shifting integers by more bits than their size, but I have yet to gather the
courage to fix it.
