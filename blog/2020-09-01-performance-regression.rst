---
title: An unavoidable performance regression
author: Troels Henriksen
description: Our generated code got a bit slower for some programs.
---

We try to make each new version of the Futhark compiler faster than
the last.  While `we sometimes make mistakes
<https://futhark-lang.org/blog/2020-07-01-is-futhark-getting-faster-or-slower.html>`_,
the the long-term trend is pretty good.  Yesterday, however, we made a
change that flat out makes some programs slower, without fixing any
bugs.  This post will serve as a bit of catharsis, and detail some of
the less enjoyable parts of compiler development.

One of the `early design flaws in Futhark
<https://futhark-lang.org/blog/2019-12-18-design-flaws-in-futhark.html>`_
is that all array sizes are 32 bit signed integers.  This limits each
array dimension to about two billion elements.  Further, since the
memory address space is ultimately one-dimensional (`NUMA
<https://en.wikipedia.org/wiki/Non-uniform_memory_access>`_
shenanigans notwithstanding), we must flatten multi-dimensional array
indexing to a one-dimensional offset.  If ``A`` is an array of type
``[n][m]i32`` in `row-major form
<https://en.wikipedia.org/wiki/Row-_and_column-major_order>`_, then
the index expression ``A[i,j]`` will be flattened to ``A[i*m+j]``.  In
Futhark, this multiplication and addition was done with 32-bit signed
integers, which limited the *total size in bytes* of multi-dimensional
arrays to 2GiB.  This size restriction has not been a big problem in
practice, as our main code generation target (GPUs) is fairly
memory-constrained, so it is rare to have single arrays that are this
large.  But GPU memory banks are growing every year, so this *will* be
a problem soon.  Further, `Minh Duc Tran
<https://github.com/HnimNart/>`_ is currently finishing up development
on a multicore CPU code generation backend, which will make Futhark
useful on more than just GPUs.  On a CPU, an array larger than 2GiB is
not exceptional.  Therefore, we changed the code generator to use
64-bit integers when computing flat offsets when generating array
indexes.  This is just the initial step, and does not truly allow
large arrays yet, but it's a good place to start, and to begin to
understand the performance ramifications.

So what are these performance ramifications?  While 64-bit arithmetic
operations possibly takes a little longer to perform, that's not the
concern.  The main problem is that they require more *registers*.  A
modern GPU consists of what NVIDIA calls `streaming multiprocessors
(SMs) <https://fabiensanglard.net/cuda/>`_.  Other vendors have a
similar design, but a different name.  Each SM can run many threads
(hundreds to thousands), which all share the same *register bank*.  An
NVIDIA RTX 2080 Ti has 256KiB of registers on every SM.  Each GPU
thread is allotted a portion of this register bank.  The size of this
portion remains unchanged throughout the run-time of the thread, but
can be set when the thread is first launched.  This is different from
CPUs, where the amount of registers available to a thread is an
unchangeable hardware detail.  However, the more registers each GPU
thread needs, the fewer threads will be able to run at the same time.
If a GPU thread needs too many registers, a compiler can perform
`spilling <https://en.wikipedia.org/wiki/Register_allocation>`_, where
register contents are temporarily copied to memory and recovered
later.  This is exactly the same idea as on a CPU, but where CPU code
can spill to the stack, which is readily cached using the deep and
sophisticated cache hierarchy of a modern CPU, spilling to memory on a
GPU can be quite expensive.  This means that *register pressure* is a
significant concern for GPU code, with sudden and somewhat
unpredictable performance cliffs.  Spilling is normally under the
control of the GPU kernel compiler, which translates C-like GPU code
to GPU machine code, but with some ability for the programmer to
influence its behaviour.

Changing the array indexing code to use 64-bit integers will increase
register pressure, which can (and does) have negative performance
implications, but the details are highly sensitive to what else is
happening in the program.  Perhaps other parts are already
register-hungry enough that the overall usage remains unchanged.  The
overall impact on Futhark-generated is difficult to quantify.  While
we have `decent tools for reproducible benchmarking
<https://futhark.readthedocs.io/en/latest/man/futhark-bench.html>`_,
our setup is not statistically robust enough to measure tiny
fluctuations of a percent or two, which is in most cases the impact I
would expect to see.  But for a few programs, the impact of using
64-bit address calculations looks more significant.

One is `OptionPricing
<https://github.com/diku-dk/futhark-benchmarks/blob/master/finpar/OptionPricing.fut>`_,
which for its largest dataset suffers a 23% slowdown on our NVIDIA RTX
2080 Ti GPU.  I did some investigation, and indeed the register usage
of its single (very large) GPU kernel has gone up slightly.  But
here's the interesting thing: *the slowdown only appears with the
OpenCL backend*.  With Futhark's CUDA backend, there is no performance
change!  I did a little bit of investigation into how the two backends
differ in the `PTX
<https://docs.nvidia.com/cuda/parallel-thread-execution/index.html>`_
(virtual NVIDIA GPU machine code) they cause to be generated, and it
appears the OpenCL compiler is keeping certain 64-bit values around in
registers for longer periods of time, while the CUDA compiler is more
aggressive about recomputing them when needed.  This is beyond what
the Futhark compiler has any control over, which is a bit frustrating.
I thought that NVIDIAs OpenCL and CUDA implementations where merely
different front-ends to the same backend compiler, so I'm not sure why
they would differ here.  A similar phenomenon occurs for a few other
benchmark programs.

Overall, there is no doubt that using 64-bit indexing slows down the
generated code - it only becomes a question of how much.  Perhaps we
can recover some of the lost performance by various
micro-optimisations, or passing different options to the GPU vendor
compilers that ultimately generate the machine code.  But at the end
of the day, we are getting slower because we are doing more work with
bigger data, and that has a cost.  That cost will only increase as we
gradually convert more of the compiler to use 64-bit sizes and
indexes.  This *sucks*, and has no immediate payoff, as all of our
current workloads *fit* in 32 bits!  But this is ultimately the right
thing to do.  However, I hope that the multicore CPU backend will
allow someone to write a fun and motivating program that uses huge
arrays, just so this work doesn't feel exclusively like a slowdown for
no practical advantage.
