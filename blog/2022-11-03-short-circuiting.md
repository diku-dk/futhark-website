---
title: Array short-circuiting
author: Philip Munksgaard
description: In-place updates without overhead
---

Hi, Philip Munksgaard here! I'm a PhD student working on Futhark, and I'm here
to talk about some of the stuff I've been working on for the past couple of
years. This blog post is a condensed version of a
[paper](https://futhark-lang.org/publications/sc22-mem.pdf) that we're headed
off to present at SC22 next week. Hope you enjoy it!

Futhark is a parallel high-level array-oriented language.  Parallelism is explicitly
expressed by the user in terms of the second-order array combinators such as
`map`, `reduce` and `scan`.  Because parallelism is explicitly described, the
compiler is able to make certain guarantees about the parallelism, such as the
guarantee that there are no data races in the code.

To see why this matters, consider this simple stencil function:

```
def stencil [n] (xs: *[n]i64): [n]i64 =
  xs with [1:n-1] = map (\i -> xs[i-1] + xs[i] + xs[i+1]) (1..<n-1)
```

In imperative languages, you might try implement this in-place, but that would
cause data-dependency errors.  Specifically, it is possible for one thread to
update an element of the array before another is able to read the original
value.  This is also called an [anti-dependency or WAR (write after
read)](https://en.wikipedia.org/wiki/Data_dependency#Anti-dependency). In a
functional language _nothing_ is in-place. Although it looks like `xs` is being
updated in the example above, semantically we are creating a new array that just
happens to have the same name. As a result, the reads and writes to `xs` must be
separated, which Futhark does by transforming the code above into the following:

```
def stencil [n] (xs: *[n]i64): [n]i64 =
  let tmp = map (\i -> xs[i-1] + xs[i] + xs[i+1]) (1..<n-1)
  in xs with [1:n-1] = tmp
```

An auxilliary buffer is introduced and is used to guarantee that updates to `xs`
do not interfere with any of the reads.  The problem is that this intermediate
array can be unnecessary.  Consider this piece of code, which updates one row of
a two-dimensional array:

```
def update [n][m] (i: i64) (xs: *[n][m]i64): [n][m]i64 =
  xs with [i] = map (+1) xs[i]
```

As programmers, it is easy to determine that this update would be safe to do
entirely in-place.  Each updated cell of the row depends only on the previous
value of that same cell, and so they can be computed and written entirely in
parallel with no risk of data-dependency errors.  Unfortunately, Futhark was not
able to do so.  The conservative nature of high-level data-race guarantees, such
as the ones Futhark provide, result in unnecessary memory being allocated and
extraneous copies.

While it may seem easy to get rid of the intermediate allocations in the example
above. After all, there are no free values in the function being mapped. But
more complex cases can make it hard for the compiler to verify that there are no
data-dependencies.  For instance, in our [NW
benchmark](https://github.com/diku-dk/futhark-benchmarks/blob/master/rodinia/nw/nw.fut),
the main loop boils down to a call like the following:

```
A[W] = map f A[P1] A[P2]
```

where `W`, `P1` and `P2` are complex slicing operations corresponding to
staggered blocks of `A`.  Even though `f` has no free variables, we still need
to guarantee that the elements written in one thread (part of `W`) does not
overlap with the reads (`P1` and `P2`) from any other thread.

The solution we have implemented in Futhark is called short-circuiting. It works
on one of Futhark's intermediate representations, `GPUMem`, which is a version
of the IR with kernel operations and memory optimizations. `GPUMem` looks
something like this[^1]:

```
let A_mem: mem = alloc (n * m * sizeof(i64))
let A: [n][m]i64@A_mem -> 0 + {(n:m), (m:1)} = segmap (i < n) {
    let B_mem: mem = alloc (m * sizeof(i64))
    let B: [m]i64@B_mem -> 0 {(m:1)} = segmap (j < m) {
        let x = i * m + j
        in x
    }
    in B
}
```

If you squint a bit, this looks like regular Futhark: The `segmap`s correspond
to `map` with an explicit index, the arrays have size-types and so on.  But we
also have a new type, `mem`, an extra `alloc` expression which returns a value
of type `mem` and arrays have some extra annotations: a memory block and an
index function. The extra annotations on arrays are used to indicate where in
memory that array resides and how the values of that array are laid out in
memory.

The short-circuit analysis works by identifying so-called "short-circuit points"
such as the last statement in this piece of code:

```
let A_mem: mem = alloc (n * n * sizeof(i64))
let A: [n][m]i64@A_mem -> 0 + {(n:m), (m:1)} = ...
let B_mem: mem = alloc (m * sizeof(i64))
let B: [m]i64@B_mem -> 0 + {(m:1)} = ...
let A[k] = B
```

and then trying to prove that it is safe to construct `B` directly in the memory
of `A` instead of relying on an intermediate buffer.  In the example above, we
call `A` the destination of the short-circuit attempt, `B` the source and `W` is
the slice that dictates how `B` should be placed inside.  The result of a
successful short-circuiting is that `B` is updated with the type `[m]i64@A_mem
-> k*m + {(m:1)}`, which in turn makes the copy on the last line a no-op.

Right now, you can see the effect of the short-circuiting pass when compiling
simple programs such as:

```
def iota_one_row [n][m] (i: i64) (xs: *[n][m]i64): *[n][m]i64 =
  xs with [i] = iota m
```

or

```
def concat_iotas (i: i64) (j: i64): []i64 =
  concat (iota i) (iota j)
```

The resulting code has fewer allocations and redundant copies, which can result
in increased performance. For instance, our NW benchmarks saw an impact of 32%
increased performance on the largest dataset on A100.

While verifying that the short-circuiting idea and implementation was valid, we
offloaded some of the algebraic computations to
[Z3](https://github.com/Z3Prover/z3), an SMT solver. Though Z3 was used to
successfully short-circuit NW and other complex programs, it could not actually
solve the NW overlap problem by itself. Instead, we used heuristics detailed in
the paper to simplify the problem and reduce it to a number of simpler
inequalities.

In order to keep Futhark free from too many external dependencies we removed the
Z3 dependency again, which means that the NW example above does not work in the
version of short-circuiting that has been merged to the current master branch of
Futhark. However, we are hoping to extend the Futhark-compiler with an
relatively simple symbolic algebra engine that will enable us to apply
short-circuiting to more programs.

[The paper](https://futhark-lang.org/publications/sc22-mem.pdf) contains more
details of this analysis and more benchmark results, and the implementation is
available in the master branch of Futhark.

[^1]: To be clear, this is not something that users ever have to write, but
something that the Futhark compiler generates based on its input. For instance,
the code above might result from processing a statement like:
```
let A = map (\i -> map (\j -> i * m + j) (0 ..< m)) (0 ..< n)
```
Although in reality, the compiler is smart enough to fuse those two maps such
that there will only be one `segmap` in the resulting IR.
