---
title: Keeping Futhark off the GPU
description: A post about a hack that forces Futhark code to run sequentially.
---

Earlier this year, Elias Smedegaard did a [BSc
thesis](../student-projects/elias-bsc-thesis.pdf) on efficiently computing
sparse [Jacobian
matrices](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant#Jacobian_matrix)
via automatic differentiation. For this post, it is not important to understand
exactly what this is, or why it is useful - it relates to [automatic
differentiation](https://autodiff.org), which I have yet to write a blog post
about. For the purpose of this post, the salient detail is that part of his
solution involves colouring a graph corresponding to the sparsity pattern of a
matrix.

Now, *optimal* graph colouring is a famously NP-hard problem, but we did not
need optimal colouring - we just needed *decent*, and implemented with an
efficient algorithm. Elias found a pretty fast greedy algorithm for distance-2
colouring (Algorithm 3.1 in [this
paper](https://epubs.siam.org/doi/10.1137/S0036144504444711) if you are
curious), but unfortunately the algorithm is inherently sequential. This is not
a problem for Futhark as Futhark supports [quite efficient in-place
updates](2022-06-13-uniqueness-types.html), but the problem is that Elias's
overall program has two steps:

1. Colour a graph.

2. Use the colouring to compute the Jacobian using massive parallelism.

We *really* want to be able to exploit parallel hardware (such as a GPU) for the
second step, while doing the first step on the CPU. It turned out that some of
Futhark's implementation choices made it awkward to do this. This blog post is
about how the problem arises, what a principled solution might be, and why we
just did a hack instead.

## How the problem arises

Futhark's compilation model is quite simple. An array needs to be in memory, and
if you use one of the GPU pipelines, then *all* arrays are put in GPU memory.
This is not because we cannot represent a mixture of CPU and GPU memory in [our
internal representation](2024-03-06-array-representation.html), but simply a
convenience of implementation. It is also a safe design, since CPU code can
access GPU memory (via costly APIs), but GPU code cannot in general access CPU
memory. That it works does not mean, however, that it is fast. Consider this
hypothetical Futhark loop:

```futhark
loop sum = 0 for i < n do
  sum + xs[i]
```

If `xs` is an array stored in GPU memory, then the loop body will contain code
for laboriously copying a single element from GPU memory to somewhere in CPU
memory, after which it can be added to `sum` (which is presumably stored in a
register). This is *slow*! Copying the single array element takes almost no
time, but setting up the communication and blocking until the copy is done has a
ruinous overhead - easily a couple of microseconds *per array element*.

A better idea would be to copy the `xs` array as a whole before the loop to
amortise the communication cost, and that is indeed a better solution that we
will get to below. But if an index into a GPU array makes it into CPU code, then
we will generate a very slow GPU memory read, which is ruinous when inside a
loop.

The distance-2 colouring algorithm implemented by Elias is essentially a couple
of nested sequential loops manipulating arrays that represent graphs and stacks.
Using a GPU backend to compile this code results in ruinous performance compared
to using the sequential `c` backend; easily three orders of magnitude or more.
If we use the `c` backend instead, then performance is excellent (basically what
you'd get if you wrote it in C), but of course then the second part of the
overall program also gets to run sequentially. As a compromise, we can use
Futhark's `multicore` backend which generates parallel CPU code, but I really
want to compute those Jacobians on the GPU!

## What a principled solution might be

Futhark does not support "separate compilation" where different parts of the
program use different backends, and that also seems a somewhat clumsy solution
to the problem. Instead, the best possible solution is what I hinted at above:
look at how (or *where*) arrays are actually used, and decide based on that
whether to put them in CPU or GPU memory - or maybe redundantly in both, if the
program calls for it.

We actually have [an
optimisation](https://github.com/diku-dk/futhark/blob/c04d9c47185c20a7ce366a3b7b203f004412dc87/src/Futhark/Optimise/ReduceDeviceSyncs.hs)
that does something similar, implemented by Philip Børgesen, which migrates
simple sequential *computation* (not data!) to the GPU if the result is only
used on the GPU anyway. The goal is to save on communication - if the work done
is trivial, and all the time is spent on copying a few array elements around,
then it is better to do the sequential work in a trivial single-thread GPU
kernel, just to cut down on communication. Sadly, this optimisation does not
apply here. First, the distance-2 colouring algorithm is too complicated (it has
loops) to trigger the optimisation, and even if we disable these checks to force
its execution on GPU, it turns out (unsurprisingly) that a single GPU thread is
ruinously slow for running such heavily looping sequential code.

In an ideal world, we can imagine a counterpart to this optimisation that moves
*data* instead of *computation*, and many of the underlying principles are
likely going to be the same. In particular, we must be careful to avoid
excessive copies, *and* also avoid keeping these copies around unnecessarily, as
they increase memory consumption. I can see the contours of what this
optimisation would look like, but I also see that it deserves more care and
attention than I have time for at the moment.

Therefore...

## The hack we did instead

Instead of trying to do something clever in the compiler, I added a new
[attribute](2020-06-28-attributes-in-futhark.html), called `#[cpu_function]`. In
my initial design, when put on a [non-inlined
function](2024-10-28-inlining.html) (usually enforced with `#[noinline]`),
`#[cpu_function]` caused the body of the function to be compiled to CPU code, as
well as all input, intermediate, and output arrays to be stored in CPU memory,
no matter which compiler backend is used. A caller of the function is
responsible for ensuring that array arguments are in the correct memory space,
and the compiler inserts code to ensure this. Here is an example usage:

```futhark
#[noinline] #[cpu_function]
def seqsum [n] (xs: [n]i32) =
  loop sum = 0 for i < n do
    sum + xs[i]
```

I did it this way because I thought it would be easy. And it almost was! Our
intermediate representation for memory has always, and by explicit design, been
able to handle multiple kinds of memory in concurrent use (specifically, our
`mem` type is parameterised by a memory space, which we also use to represent
GPU shared memory). However, the fact that we have never before widely
*exercised* this functionality caused some issues that had to be solved. In the
spirit of things, the solutions I picked were whatever was most expedient, until
I have time to do *the principled thing* sometime in the future.

One issue I encountered is that our code generation for GPU kernels assumes that
all arrays referenced in the body were already in GPU memory, which is no longer
the case. For example, imagine this is our program:

```futhark
#[noinline] #[cpu_function]
def frob (n: i64) =
  iota n

entry main n = let xs = frob n
               in map (\x -> x + 1) xs
```

The `map` in `main` gets compiled to a GPU kernel, but the array `xs` is in CPU
memory due to `#[cpu_function]`! This is not valid. The *principled* solution is
to actually look at which arrays are being accessed inside GPU kernels and
copying them to the GPU if necessary, but doing this *right* is actually not so
easy:

1. If a GPU kernel is called in a loop, we should not copy for each iteration of
   the loop.

2. ...but we should also keep the array copies around for as short a time as
   possible.

My solution to this was to tweak the meaning of `#[cpu_function]` a bit: while
the *input* and all intermediate results are in CPU memory, the *result* will be
in GPU memory, achieved by a copy at the end of the function. This essentially
means that the compiler can largely keep assuming that all arrays are in GPU
memory, as this will still be the case for everything except those functions
marked with `#[cpu_function]`.

...of course, those functions then turned out to be a problem. The GPU backends
assumed that all `iota` and `replicate` operations (which are IR primitives)
would produce results in GPU memory, which is not the case inside a
`#[cpu_function]`. [Fixing that was not so
difficult](https://github.com/diku-dk/futhark/blob/42808b162f6b85b76f89ca7eed687d27e952ef8f/src/Futhark/CodeGen/ImpGen/GPU.hs#L267-L270),
but it is likely that similar bugs lurk elsewhere.

Finally, a question arises of what should happen when you `map` a
`#[cpu_function]`. I arbitrarily decided that we then ignore `#[cpu_function]`
and generate a normal [parallel lifted
function](2026-07-31-full-flattening.html), but an argument could also be made
that this should produce a sequential function somehow.

## Where we are now

While `#[cpu_function]` is a somewhat sharp-edged hack, it turned out to solve
the immediate problem: we can now do graph colouring using very efficient
sequential code, *and* also do parallel things within the same Futhark program.
In particular, graph colouring is now a very small part of the overall run-time,
which is dominated by the subsequent numerical work. The `#[cpu_function]`
technique is coarse in the sense that calling these functions involves expensive
copies, but because they are *bulk* copies instead of per-element, the overhead
is not so great.

The modifications required to the compiler were somewhat minor, and most can be
categorised as bug fixes. Even in that glorious principled future where the
Futhark compiler becomes able to automatically determine where an array should
be stored, it is likely we will keep this attribute around as a useful hint.
