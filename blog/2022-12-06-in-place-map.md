---
title: In-place mapping and the pleasure of beautiful code nobody will ever see
author: Troels Henriksen
description: We implemented a fancy new optimisation but perhaps forgot the original simple motivation.
---

Last month [Philip wrote about our fancy new short-circuiting
optimisation](https://futhark-lang.org/blog/2022-11-03-short-circuiting.html),
which analyses the program and modifies it such that values are
constructed *in-place*.  The intent is to avoid the copies that
usually result from functional programming, where a value is assembled
in memory *over there*, and then afterwards copied to where it is
needed *over here*.  Just consider two separate expressions that both
produce an array each, which are then concatenated.  Short-circuiting
modifies those two expressions such that the arrays are produced
adjacent to each other in memory, meaning the concatenation becomes a
no-op.

Short-circuiting is a very general technique, and the concatenation
example is just one thing it can do.  The original inspiration for
this line of work came from [Cosmin
Oancea](http://hjemmesider.diku.dk/~zgh600/), the spiritual father of
the Futhark project as a whole, who really wanted to have certain
guarantees of the code that would be generated from a Futhark program.
For example, for simple expressions like `map f xs`, he desired this
`map` to be operationally *in-place*, meaning the result array should
be stored in the same memory as that occupied by `xs`.  This is
possible when two things hold:

1. `xs` is not used afterwards (nor anything aliased with `xs`).
2. The type of the result is the same as the type of `xs` (put another
   way, `f` must have type `t -> t` for some `t`).

A compiler could check for this and make the `map` in-place when
possible.  Eventually this line of thinking grew into what became
short-circuiting, which handles much more fancy cases ([read the blog
post](https://futhark-lang.org/blog/2022-11-03-short-circuiting.html)
or [the paper](https://futhark-lang.org/publications/sc22-mem.pdf)).
However, while attending [SC22](https://sc22.supercomputing.org/),
where this paper was presented, I discovered that along the long and
difficult journey to publishable results, we had actually forgotten
our original simple motivating example: `map`s were *not* performed
in-place!

Fortunately, [Philip](https://munksgaard.me/) was able to quickly
address the omission and [support this optimisation within the
framework of
short-circuiting](https://github.com/diku-dk/futhark/pull/1766).  It
was nice to see that the fundamental approach was flexible enough to
also cover this case, even though we had forgotten all about it.

But what is the impact of this supposed optimisation in practice?
Consider a function like the following:

```Futhark
def main (xs: *[]i32) = map (+1) xs
```

As the parameter `xs` is marked *unique*, the function has ownership
of it.  This means that the `map` can be performed in-place, and
indeed it is.  This does not affect the total memory traffic: we still
have to read and write one `i32` for every array element.  The only
advantage of operating in-place is that the [memory
footprint](https://en.wikipedia.org/wiki/Memory_footprint) is cut in
half, as we do not need to allocate memory to store the result.
Lowering memory usage can be a goal in itself, but it does not always
significantly affect performance.  But let's see!

On an input array with 100 million elements, on an A100 GPU, the
in-place `map` runs in 798μs, while the out-of-place version runs in
811μs.  Essentially nil impact.  However, if we ask Futhark to
generate parallel *CPU* code, the in-place `map` runs in 7152μs and
the out-of-place `map` in 15839μs.  That means the in-place `map` is
about 2x faster!  The reason for this dramatic speedup is that when
the memory footprint is lowered, we can fit more data in the CPU
cache.  While GPUs have caches as well, they are less important, and
instead depend on latency hiding and high-throughput memory buses to
obtain performance.

This is of course an absolutely best-case scenario, where the
footprint is cut in half.  The impact on real programs is smaller,
although still noticeable.  But the real reason I enjoy this
optimisation is harder to quantify: it is aesthetic rather than
numeric.  I take pleasure in looking at the generated code and seeing
this:

```C
for (int64_t SegMap_i_6133 = start_6130; SegMap_i_6133 < start_6130 + n_6132; SegMap_i_6133++) {
  int64_t slice_6134 = dz2080U_6078;
  int64_t gtid_6120 = SegMap_i_6133;
  int64_t remnant_6135 = SegMap_i_6133 - gtid_6120;
  int32_t x_6106 = ((int32_t *) mem_6124.mem)[gtid_6120];
  int32_t defunc_0_f_res_6107 = add32(1, x_6106);
  ((int32_t *) mem_6124.mem)[gtid_6120] = defunc_0_f_res_6107;
}
```

Looking past the intermediate bindings and the compiler-chosen names,
this just looks *right*.  This is how I myself would write this
program in C: read an element of the array, do something to it, and
put it back where I got it from.

My colleague [Martin Elsman](https://elsman.com/) maintains the
[MLKit](https://elsman.com/mlkit/) compiler.  He recently made some
improvements to code generation that made it produce simpler
assembly - things like avoiding superfluous moves and jumps-to-jumps.
As expected, this had no performance impact on modern speculating,
superscalar processors, but Martin expressed significant pleasure at
reading the now-cleaner emitted code.

I wonder how many compiler developers feel this way.  We often say
that code is "written for humans to read" and therefore should be
readable, but the code produced by a compiler is usually destined for
nothing but execution.  Still, I like knowing that the unseen
transient code produced by the compiler contains a spark of elegance
and simplicity, even if it is sometimes well hidden and no user will
ever notice.
