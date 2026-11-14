---
title: Fixing failure handling in parallel code
description: An explanation of recent fixes to various edge cases in Futhark's run-time failure handling in parallel code.
---

Futhark is unusual among GPU-targeting languages in providing full support for
dynamic safety checks, such as bounds checking, using the approach described in
the paper [*Bounds Checking on GPU*](../publications/hlpp20.pdf) (and [companion
blog post](2020-07-13-bounds-checking.html)), which still stands as perhaps my
favourite paper. While the technique presented in the paper remains correct
(except for a small detail, to be covered below), a recent attempt to fix a
minor oversight revealed a whole bunch of unconsidered edge cases ([just like
last time](2026-09-22-aliasing.html)) that I have spent some time fixing. This
post discusses the issues I found and how they were fixed. The post is mainly
meant as catharsis on my part as debugging these things was not very fun, and so
the technical details may seem tedious, but the takeaways at the end should be a
bit more widely interesting.

## Why GPU failure handling is tricky

Safety checks, such as bounds checks, are tricky to implement on GPUs because it
is difficult to terminate a running GPU kernel from within the kernel. It is no
great trouble for a single thread to notice that something has gone wrong, nor
for that matter to terminate itself, but current GPU APIs do not provide any way
for this thread to ask that the entire kernel terminates. This causes some
trouble due to the nature of concurrency on the GPU.

Fundamental synchronisation primitives, such as barriers, require *all* threads
in a thread block to eventually reach that barrier if *any* of them do.
Violating this rule is called "barrier divergence", which is basically undefined
behaviour (details can vary among GPU programming models). If one of those
threads has failed out due to a failed bounds check, then we are violating the
rules of the programming model. A common consequence is an infinite wait, but
another is that the GPU program is terminated in some catastrophic manner by the
GPU driver, and related GPU state lost.

Futhark does not provide error handling from *within* Futhark programs, but it
promises that errors can be observed [from the
*outside*](2022-07-01-how-futhark-talks-to-its-friends.html), and that if an
entry point function fails, then we can still call other entry points (or the
same entry point again with presumably different arguments), or access data
stored in Futhark values. This guarantee cannot be upheld if every bounds check
failure runs the risk of catastrophic termination by the GPU driver.

Thus it is important that Futhark programs, no matter how wrong they are at the
Futhark level, *never* violate the correctness properties of the underlying
low-level programming model.

The basic technique we use to accomplish this is fairly simple: when a thread
fails a safety check, it jumps ahead (basically a `goto`) to the next "error
synchronisation point", which is a point in the code where all threads
collectively check on each other, and if one of them has failed, then they all
fail together. These synchronisation points usually coincide exactly with the
barriers used for cross-thread communication, so they are also natural places to
check. We have to be careful never to jump *past* any barriers, as this would
also induce barrier divergence. The rule is that all threads must reach the
*same* barrier, not *some* barrier.

We further apply some optimisations to handle various special cases, and make it
cheap to check for failures - read [the paper](../publications/hlpp20.pdf) or
[blog post](2020-07-13-bounds-checking.html) for details. However, I recently
realised that there are some exotic cases where we did not properly handle all
possible edge cases. This is not due to a conceptual problem in our approach,
but just some (important!) details for certain constructs.

## The single-pass scan issue

GPUs execute threads by splitting them into *thread blocks* of some fixed size.
In the traditional view, we permit only communication between threads in the
same thread block, resembling the [bulk synchronous parallel
model](https://en.wikipedia.org/wiki/Bulk_synchronous_parallel). Otherwise, to
observe the effects of another thread block, we must wait for the entire kernel
to finish, and then launch a new one.

On GPU, Futhark implements the `scan` primitive using a fancy [decoupled
lookback
scan](https://research.nvidia.com/sites/default/files/pubs/2016-03_Single-pass-Parallel-Prefix/nvr-2016-002.pdf).
One of the tricks behind the performance of this algorithm is that it views the
GPU as a standard concurrent processor rather than as a stream processor, and
performs direct communication between thread blocks. The main form of
communication is that during the algorithm, a thread block will wait for a
preceding thread block to publish its partial result, which happens by setting a
flag in global memory. If that thread block fails during execution, then the
flag will never be set, causing infinite waiting.

The problem here is not that we are violating some principle of GPU programming
(at least not more than the decoupled lookback scan does in the first place),
but that our implementation of thread failure assumed that the only challenge
was shutting down a single thread block safely. However, when we have
cross-thread-block synchronisation, then we must also make sure that we have
properly set any flags and preserved invariants such that other thread blocks
can make progress. Unfortunately, this requires [specialised programming for
every parallel primitive](https://github.com/diku-dk/futhark/pull/2605),
although at the moment scans are the only place we do this kind of cross-block
synchronisation. At the compiler implementation level, it involved extending our
["error synchronisation point"
construct](https://github.com/diku-dk/futhark/blob/772247f53b84a4dedc0ff64f902d9af1013ff623/src/Futhark/CodeGen/ImpCode/GPU.hs#L213-L219)
to also support running site-specific cleanup code in case of errors.

## The lock issue

At certain times, most notably when computing [generalised
histograms](../examples/histograms.html), Futhark needs to perform an atomic
update to some array element. Viewed as imperative pseudocode, the idea is
pretty simple:

```
a[i] = f(a[i], x)
```

Here `f` is some function and `x` is some value used for the update. Many
threads may be trying to update `a[i]` concurrently, so we need synchronisation.
Depending on what `f` does, the Futhark compiler knows various tricks for
generating code for the update. As an example, if `f` corresponds to an
operation directly supported by a dedicated atomic instruction in hardware (such
as addition) then that instruction is used. However, in the worst case, for the
most convoluted functions, the compiler falls back to using a spinlock to ensure
mutual exclusion:

```
while (true) {
  bool taken = try_to_take_lock();
  if (taken) {
    a[i] = f(a[i], x);
    release_lock();
    break;
  }
}
```

There are of course *interesting* challenges related to memory fences and lock
contention, but those came as no surprise. I had however forgotten to think
about the case where the function `f` fails. In that case we jump out of the
critical section and into the next error synchronisation point, which leaves the
lock taken, meaning other threads will spin forever.

[The solution](https://github.com/diku-dk/futhark/pull/2608) is basically to do
what you do when writing code like this by hand: catch/detect errors, and clean
up the acquired resources (the taken lock). This turned out to require an
entirely new concept in Futhark's imperative IR ("in case of failure, do this
before propagating the failure"). Previously, the only cleanup-on-error code we
did was freeing memory, which was done in an implicit way during final code
generation. I suppose this facility may also come in handy in other cases.

## The small detail

I also realised that there is actually a small error in the bounds checking
paper. One of the contributions of the paper is the observation that GPU kernels
are launched asynchronously, and it would be very slow for the CPU to check for
failure in one kernel before launching the next one. Hence, all kernels are
launched optimistically, but given a *failure prologue* that checks whether a
previous kernel has failed. It looks like this:

```C
if (*global_failure) {
  return;
}
```

Here `global_failure` is a pointer to some word in memory that is set to a
non-zero value whenever a thread fails. The problem is that this check is
subject to a race condition: GPU threads do not execute in lockstep, and so
`global_failure` may be set by another thread in the current kernel. Depending
on arbitrary scheduling decisions, a thread may then notice the error and shut
down, whilst other threads in the thread block continue on. I have not yet fixed
this issue, since I am curious about whether I can actually provoke this problem
in practice, or whether there is something that prevents it from occurring.

## Takeaways

This is a bit of an odd post, that is fundamentally about fixing certain obscure
edge cases in Futhark's code generator. I think it is worth reflecting on why
these bugs went undiscovered for so long, and why I bothered fixing them.

First, these bugs only occur for complicated reduction or histogram operators
that contain potential failures, such as array indexing or programmer-specified
`assert`s. These are *very rare* - most operators are extremely simple. It was
Martin Elsman's [library of monoid
combinators](https://github.com/diku-dk/monoidal) that made me take a look at
these cases in the first place, and that library is, I think, the closest to
"real usage" of complicated operators.

Second, the bugs only trigger when there is an *actual failure* when executing
the operators. If they do not fail, the previous code was fine. Since Futhark
does not provide any way to handle failures from within programs, input checking
is always done explicitly up front, rather than relying on bounds checking or
`assert`s inside the core logic. These safety checks are considered stopgaps,
not an idiomatic way to express program logic. This means most Futhark
programmers debug their code using the interpreter or sequential backends, and
don't even bother to run it on a GPU (or multicore CPU) until they believe it
works.

So why even fix these bugs? As I mentioned in the intro, my impression is that
most GPU-targeting languages do not make any effort to perform bounds checking
of this kind! Why is it so important for Futhark? I suppose the real reason is a
mixture of stubbornness and pride. I *like* that Futhark has the same semantics
no matter what you run it on. I think languages should be [defined in terms of
their semantics](2025-05-07-implement-your-language-twice.html) rather than
whatever is expedient. I think languages should keep their promises in the small
as well as in the large, and if necessary, not make promises that they cannot
keep. I don't think Futhark is all the way there, but we are at least a bit
closer than we used to be. Of course, I have no evidence that this makes a
language more useful or popular, but it certainly makes me more intellectually
satisfied with it.
