---
title: Speeding up GPU copies by permuting iteration spaces, or, optimising the identity function
description: In some cases, making semi-costly decisions at run-time can be used to speed up very costly copies.
---

Following the recent posts on such riveting topics as [integer
division](2026-10-14-defining-division.html) and
[addition](2026-10-21-integers-how-ought-they-work.html), we turn our attention
to another exciting topic: copying bytes. Most compilers end up emitting code
that copies data around, and in most cases this means very large amounts of very
small copies, such that any attempt at run-time cleverness is likely
counterproductive. However, array languages are usually in the situation of
dealing with a relatively small number of operations, that on the other hand are
large, which means some interesting optimisations are possible. In this post I
will explain how Futhark now makes certain copies a little faster, by exploiting
information about array layouts that are only available at run-time. This has a
small impact on some programs, but the technique is probably more fun than
useful.

## Futhark's array representation

I previously wrote a [detailed post](2024-03-06-array-representation.html) on
how Futhark represents array layouts, but I will repeat the salient parts in the
following.

The fundamental question is how to map the *n*-dimensional index space of an
*n*-dimensional array to the flat index space of computer memory. We need an
*index function* from a tuple of *n* indexes to a single integer, representing a
relative offset in some memory block. These index functions can in principle be
essentially arbitrarily complicated, but because we want to both analyse them
statically and represent them dynamically, Futhark uses a structured (and
limited) representation known as *linear memory access descriptors* (LMADs).
Simplifying slightly, an LMAD for an *n*-dimensional index space consists of *n*
pairs of integers, one per dimension, with each pair denoting a *size* and a
*stride*. The sizes describe the size of each dimension, and we will return to
the strides in a moment. In total, an LMAD *L* for a *d*-dimensional space is
written as follows:

```
L = [(n_1, s_1), ..., (n_d, s_d)]
```

A *d*-dimensional LMAD can be applied to *d* integers as follows:

```
L(i_1, ..., i_d) = i_1*s_1 + ... + i_d*s_d
```

That is, each index is multiplied with the corresponding stride, and the whole
thing summed. If we have a two-dimensional array `A` represented by the LMAD
`L`, then to index `A[i,j]` we compute an offset with `L(i,j)` and use that as
an address relative to whatever memory `A` is stored in (multiplied with an
element size, of course).

Here is an example of an LMAD describing an array of shape `[4][5]` in row-major
order:

```
[(4, 5), (5, 1)]
```

And here it is in column-major order:

```
[(4,1), (5, 4)]
```

We can see that when we go from index `[i][j]` to `[i+1][j]`, then the address
increases by 5 when using row-major, and by 1 when using column-major. This is
as expected. LMADs thus provide a convenient way to decouple the logical shape
of an array from its physical element order. We can also do things like slicing,
reshaping, and transposition in a copy-free manner, simply by fiddling with the
LMADs instead. (Slicing also requires a single additional offset in addition to
the dimensions, but that is immaterial for this post - read the one linked above
if you are curious.)

We don't need the LMAD sizes for the index operation, but they are useful when
operating on multiple elements. In particular, they describe the size of the
array. If we have two arrays `A` and `B` of the same shape, stored in memory
`AM` and `BM` and with LMADs `AL` and `BL`, then we can copy `A` to `B` using a
loop like this:

```
for i_1 < n_1:
  ...
    for i_d < n_d:
      BM[BL(i_1, ..., i_d)] = AM[AL(i_1, ..., i_d)]
```

If we assume that LMADs are injective (each index maps to different offset,
which in particular bans zero strides), then the above loop has no dependencies,
i.e., all iterations can be executed in parallel. The Futhark compiler only
generates such LMADs, so we will assume that henceforth.

LMADs cannot represent all possible layouts, but they can represent any
permutation of dimensions, which is the most important case for our purposes.
They are used in the compiler itself, where the components are (usually)
symbolic rather than known integers, but LMADs also exist at run-time, where
they are used to dynamically optimise array copies, instead of always doing the
naive loop described above.

## Optimising LMAD copies

Consider again that we have two arrays `A`/`B` with LMADs `AL`/`BL` in memory
blocks `AM`/`BM`, and we have to copy `A` to `B`. We can certainly do it with
the manual loop, but odds are that a platform-provided `memcpy()` (or GPU
equivalent) is faster because it is written to take advantage of hardware-level
features, like vectorisation. It would be good if instead of doing a manual
loop, we could just emit `memcpy(B, A, m)`, with `m` being the total size in
bytes. This is possible exactly when `A` and `B` have the same layout, and the
LMADs describe *contiguous* arrays, in the sense that there are no "gaps" in
memory between elements.

Detecting such contiguous arrays is possible with run-time analysis.
Specifically, two neighbouring dimensions `(n_1,s_1)`,`(n_2,s_2)` in an LMAD can
be *collapsed* into a single dimension when `s_1=s_2*n_2`, and the result is a
dimension `(n_1*n_2,s_2)`. If there exist *any* permutation of the LMAD
dimensions such that we can collapse it into a single dimension, then the LMAD
is contiguous (we can also ignore singleton dimensions). This can be done with a
worst-case exponential search. Doing such a search on every copy might be a bad
idea for most languages, but note that the search is exponential in the rank of
the array, and usually arrays in Futhark programs have very few dimensions.
Importantly, *the rank is a constant* no matter the size of the array. It is not
a big deal to burn a few thousand instructions before copying an array with
thousands (or billions) of elements.

Another important special case is to detect *transpositions*, which occurs when
we are copying between different layouts in a way that corresponds to a
transposition of a matrix. These are important cases because a naive
transposition has poor locality on all hardware I know of: either the reads or
writes will be accessing with a stride. However, with careful programming that
exploits the memory hierarchy carefully, it is possible to implement
transpositions that are almost as fast as a full-speed memory copy. (Details are
out of scope for this post.)

Transpositions can be detected by collapsing contiguous dimensions until only
two dimensions are left in each LMAD, and then checking whether they are
transposes of each other. In fact, Futhark is a little more clever and allows
*batched* transpositions (or `map transpose` as I prefer to call it), which
allows an additional outer dimension that is unchanged, and additionally does
not require the "matrices" to be contiguous with each other.

Transposes are quite important, because Futhark inserts them in the program to
improve the locality of array traversals based on what the program actually
does. This would obviously not work if those transpositions were not implemented
efficiently. The capability to optimise data representation is one of my
favourite consequences of using high level programming languages.

The curious can [see the implementation of Futhark's host memory copy
function](https://github.com/diku-dk/futhark/blob/53a520012fb9733707c4cad3986b0e75c4a6d9ef/rts/c/copy.h#L220-L245)
(macro-ized for different element types), which covers the cases discussed
above.

## The new case

Perhaps there are more interesting structures we will detect in the future, but
at some point, we must face the possibility that the LMADs involved are just too
complicated to make any sense of them, and fall back to copying elementwise with
a loop. This is not the end of the world, as at least that loop is parallel, but
I realised that we can (heuristically) do *slightly* better. Consider again the
general loop for copying between arrays described by LMADs:

```
for i_1 < n_1:
  ...
    for i_d < n_d:
      BM[BL(i_1, ..., i_d)] = AM[AL(i_1, ..., i_d)]
```

Suppose we want to run this on a GPU. Since GPUs do not support nested
parallelism ([hence all this](2026-07-31-full-flattening.html)), we transform
the above into a single loop:

```
for i < n_1 * ... n_d:
  (i_1, ..., i_d) = unpack_flat_index(i)
  BM[BL(i_1, ..., i_d)] = AM[AL(i_1, ..., i_d)]
```

That `unpack_flat_index` function is the usual mess of divisions, but it is not
our concern here. Let us suppose that this flat loop is however a
straightforward flattening of the original nested loop (same lexicographic
order), and suppose we assign one GPU thread to each iteration of the loop. We
have not fundamentally changed the structure of the loop by adding new
dependencies, so we can still change the iteration order in any way we wish, in
order to perform locality optimisations. (Note that this follows from the
injectivity of the destination LMAD, but does not hold for arbitrary LMADs.)

### Coalescing

A GPU executes threads by bundling consecutive threads into groups called
*warps* or *wavefronts* of typically 32 or 64 threads, which execute in lockstep
(fine print applies, it doesn't matter for this explanation). Intuitively, the
threads of a warp execute the same instruction at the same time. The hardware
services these accesses using one or more memory transactions, each covering a
contiguous block of memory. If the different threads access nearby locations,
fewer transactions are needed. We call this *coalesced memory access*.
Otherwise, multiple transactions are necessary before all threads have been
serviced. We call this *uncoalesced memory access*. Ensuring coalesced access is
one of the most fundamental performance requirements when doing GPU programming.
The rule of thumb is as follows: *neighbouring threads should access
neighbouring elements of arrays*.

### ~~Ensuring~~ Hoping for coalescing

In [Futhark's GPU kernel for general LMAD
copying](https://github.com/diku-dk/futhark/blob/53a520012fb9733707c4cad3986b0e75c4a6d9ef/rts/opencl/copy.cl),
the index space is flattened "column-major" in a sense, such that neighbouring
threads process neighbouring *outermost* indexes. That is, when copying a
two-dimensional array, two threads will be copying elements `[i,j]` and
`[i+1,j]`. Note that this is still the logical iteration order, whose mapping to
the *physical* iteration order is given by the LMADs of the source and
destination arrays.

This inspires an optimisation where before copying, we permute the dimensions of
the LMADs such that they are sorted in order of increasing stride by the source
LMAD (and secondarily destination LMAD). The goal is to minimise the stride
between neighbouring threads. Ideally we hope for perfect coalescing, but even
strides greater than 1 can be useful. We prioritise locality for the source
array because uncoalesced reads seem to be more costly than uncoalesced writes
on the GPUs I have tested. [We sort with a basic selection
sort](https://github.com/diku-dk/futhark/blob/53a520012fb9733707c4cad3986b0e75c4a6d9ef/rts/c/gpu.h#L533-L559),
which has quadratic complexity, which is again a small price to pay if we can
improve locality.

To summarise the reasoning:

1. The general copy kernel assigns adjacent GPU threads to adjacent indices in
   the outermost dimension.

2. The outermost dimension is not necessarily the one with the smallest stride
   in memory.

3. Permuting the iteration dimensions can make adjacent threads access adjacent
   memory locations.

4. The permutation is selected dynamically, based on the source and destination
   LMADs, via sorting based on strides.

For example, take the case where we have a two-dimensional array `xss` of shape
`[n][m]`, and we are doing a slice that removes the first element of every row:
`xss[:, 1:]`. By default, Futhark stores arrays in row-major order, so `xss` has
the LMAD `[(n,m),(m,1)]`, and the slice consequently has LMAD `[(n,m),(m-1,1)]`.
This by itself is not a problem, as simply performing a slice is free and
involves no copying. But there are cases (such as values returned by entry
points to the outside world) where Futhark insists on a "normalised" array
representation, which means row-major order with no gaps.

If this slice is to be turned into a normalised row-major representation, we
have to copy to an LMAD `[(n,m-1),(m-1,1)]`. Note that the source array is not
contiguous, so we must use the general LMAD copy kernel, and since the stride of
the outermost dimension is `m` (likely large), we have uncoalesced reads and
writes. If we sort the LMAD dimensions by their stride (using the same
permutation *for both LMADs*), we instead get (assuming `n>1,m>1`):

* Slice LMAD: `[(m-1,1),(n,m)]`

* Destination LMAD: `[(m-1,1),(n,m-1)]`

Now we have a unit stride in the outermost dimension, and we obtain perfectly
coalesced access.

## Impact

So what is the impact of this change? I will state up front that the overall
impact across the entire [Futhark benchmark
suite](https://github.com/diku-dk/futhark-benchmarks) was modest, as most
programs did not issue copies with bad locality. The few exceptions were limited
to at most 10% speedup, as I recall. But when your fancy new hammer doesn't make
a difference for pre-existing nails, why not just forge some new nails? Consider
[these Futhark
functions](https://github.com/diku-dk/futhark-benchmarks/blob/c9e2ccca7cf3efa698519f59b9adbf626b98098f/micro/copy.fut):

```Futhark
entry slice_2d [n] [m] (xss: [n][m]i32) = copy xss[:, 1:]

entry slice_3d [n] [m] [k] (xsss: [n][m][k]i32) = copy xsss[:, :, 1:]

entry reversed_rows [n] [m] (xss: [n][m]i32) = copy (map reverse xss)
```

These functions are designed not to match any of the recognised LMAD patterns
(`memcpy()` or transpose), and so would fall into the general pattern. Without
the sorting-based optimisation, I obtain the following performance with the
[`hip` backend](2024-07-17-opencl-cuda-hip.html) on my AMD RX 7900 XT GPU:

```
copy.fut:slice_2d (no tuning file):
[8192][8193]i32:          17561μs (95% CI: [   17500.2,    17606.0])
[65536][1025]i32:         36062μs (95% CI: [   36026.8,    36096.0])
[1048576][65]i32:         27241μs (95% CI: [   27091.3,    27366.1])
[16777216][5]i32:          3466μs (95% CI: [    3464.8,     3467.5])

copy.fut:slice_3d (no tuning file):
[256][256][1025]i32:      48624μs (95% CI: [   48537.3,    48735.5])
[65536][32][33]i32:       57708μs (95% CI: [   57610.5,    57816.4])

copy.fut:reversed_rows (no tuning file):
[8192][8192]i32:          12468μs (95% CI: [   12438.8,    12537.8])
[16777216][4]i32:          3071μs (95% CI: [    3069.5,     3071.6])
```

With sorting, I get the following:

```
copy.fut:slice_2d (no tuning file):
[8192][8193]i32:            971μs (95% CI: [     964.5,      979.8])
[65536][1025]i32:           992μs (95% CI: [     990.8,      993.8])
[1048576][65]i32:           990μs (95% CI: [     988.5,      991.9])
[16777216][5]i32:          1010μs (95% CI: [    1008.6,     1011.6])

copy.fut:slice_3d (no tuning file):
[256][256][1025]i32:       1366μs (95% CI: [    1363.9,     1368.1])
[65536][32][33]i32:        1364μs (95% CI: [    1361.7,     1366.9])

copy.fut:reversed_rows (no tuning file):
[8192][8192]i32:            996μs (95% CI: [     994.0,      998.3])
[16777216][4]i32:           999μs (95% CI: [     997.3,     1000.7])
```

Note the huge speedup in some cases - about *36x* for `slice_2d`. And I wouldn't
even say these programs are particularly contrived! The performance is not great
in absolute terms (about 514 GiB/s), but this has certainly made me curious
about how to further speed up copying in the compiler. Some part of me says that
this is silly in a sense, as we are basically optimising the identity function,
but it turns out that just moving data around is a pretty important thing to get
right.

I'm not sure what's next. This post has mostly focused on the GPU case, but
similar effects could be expected for our CPU backends. There currently is a
DIKU student working on speeding up parallel transposition on CPU, so perhaps
that will be the topic of a post in the future.
