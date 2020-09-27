---
title: Retrospective on an implementation of the Poseidon hash function in Futhark
author: Troels Henriksen
description: A programmer from the Filecoin project wanted to use Futhark, which in the end made the compiler much better.
---

Futhark is not a very widely used language, and since the compiler
contains no telemetry, my main source of information about usage is
when users ask for help or report bugs.  From these interactions, it
appears that most Futhark users are individual programmers, often
scientists or hobbyists, who implement algorithms that have not
previously had high-performance implementations.  Usually, the users
of the programs are the same as their programmers.  This is no great
surprise - one of the most important strengths of Futhark is that
algorithmic experimentation is much easier than in low-level languages
like CUDA, while still running much faster than high-level languages
such as Haskell.  It seems to be a good balance.  In this post, I will
discuss the experience of using Futhark for something quite different:
namely implementing a previously defined algorithm, one with quite
straightforward parallelism, and integrating the Futhark code in a
nontrivial Rust context.

In January, `porcuquine <https://github.com/porcuquine>`_ from the
`Filecoin <https://filecoin.io/>`_ project wrote me and asked for
advice on a potential use of Futhark: implementing the `Poseidon hash
function <https://www.poseidon-hash.info/>`_, where the most intensive
computation involves operations on elements of `finite fields
<https://en.wikipedia.org/wiki/Finite_field>`_, which requires large
integers.  They already had an `implementation in OpenCL
<https://github.com/filecoin-project/ff-cl-gen>`_, including
`hand-written GPU assembly
<https://github.com/filecoin-project/ff-cl-gen/blob/bde199506199032d89a7ac1ac46266cd9ca974ce/src/cl/common.cl#L20-L29>`_.
I like to think I was honest in my response, where I explained that
while Futhark is probably suitable, its strengths are mainly useful
for programs with complex parallel structure, and that its performance
probably cannot match hand-written primitives.  Fortunately, this was
not enough to scare off porcuquine, and over the next several months,
he managed to find a great deal of limitations and bugs in the
compiler, which I mostly promptly fixed.  I think my bug-fixing ethos
is quite *guilt-driven*, as I implicitly suggest that people should
use Futhark (because I put it out there and talk about it), and then I
feel guilty when they run into problems due to my errors.  None of the
problems were conceptually deep or concerned the language itself, but
rather were all implementation restrictions that just had not been
apparent before.  `Porcuquine's implementation
<https://github.com/filecoin-project/neptune-triton>`_ (hereafter
referenced as *neptune-triton*) simply touched the compiler in ways it
had never been touched before.  The rest of this post is a summary of
the most interesting fixes and improvements that were made as a result
of this process.  Ultimately, while only time will tell whether
Futhark has been useful to Filecoin, I can say that Filecoin has
definitely been useful to Futhark.

New primitives
--------------

The first challenge, and the one that was easiest to resolve, was
adding language primitives for `fused multiply-add
<https://en.wikipedia.org/wiki/Multiply%E2%80%93accumulate_operation>`_.
Porcuquine `did this himself
<https://github.com/diku-dk/futhark/pull/866>`_, and the new
primitives were made available in version 0.15.1.  With the OpenCL and
CUDA backends, these primitives map directly to the corresponding
operations exposed by the GPU APIs.  On the other backends, and in the
interpreter, they are simply implemented as well as possible.

After this, things became more difficult.

Static arrays
-------------

The core of the Poseidon hash function is computation on elements of
finite fields - basically integers (far) more bits than directly
supported by the machine.  For finite fields, the number of digits (or
"limbs") needed for each field element will be known at compile-time,
in contrast to `common bignums
<https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic>`_,
which can grow to any size and generally may need allocation at
inopportune times, which makes them a poor fit for GPUs.

However, while the number of digits in each field may be finite, we
may still need different fields with different sizes, so to avoid
duplicating code, we should write a generic implementation that can be
instantiated with the desired size.  Futhark's `ML-style module system
<2017-01-25-futhark-module-system.html>`_ is `an excellent fit here
<https://github.com/filecoin-project/fut-ff/blob/801985c67955a9baf29a34128f548e0eb7a9a1e5/lib/github.com/filecoin-project/fut-ff/field.fut#L74>`_.
But an important question must still be answered: how do we represent
the elements of the field?  The obvious choice is an array of 64-bit
digits, but for this application, this causes trouble, as the arrays
would be quite small (`the main field definition uses just four digits
<https://github.com/filecoin-project/neptune-triton/blob/e1ebadc5643dccad9c05d08ea634975a6805886e/library/poseidon.fut#L14-L18>`_).
This is by itself not a problem, but the futhark compiler generally
assumes that arrays will be large, heap-allocated, and worth
parallelising.  Thus, small array constant-size arrays is usually a
code smell, as it can cause significant and unnecessary overhead.  It
would be much better to represent these field elements as essentially
tuples, and sequentialise operations on them.  `I have written
previously about why Futhark does not automatically do this
<2019-01-13-giving-programmers-what-they-want.html>`_, and the
solution is the same as usual: Parameterise over the *representation*
of field elements, and use the `vector
<https://github.com/athas/vector>`_ library for efficient small
arrays.  Poseidon did need the library to gain some new facilities,
notably ``foldl`` and ``foldr`` functions to implement `carry-based
addition
<https://github.com/filecoin-project/fut-ff/blob/801985c67955a9baf29a34128f548e0eb7a9a1e5/lib/github.com/filecoin-project/fut-ff/field.fut#L267-L285>`_.
These functions result in quite beautiful and quite efficient fully
unrolled code.  The nice thing here was that this improvement
concerned only library code, not the compiler.  It's nice that Futhark
has become robust and expressive enough that not every problem has to
be fixed with compiler hacking.

Costly constants
----------------

While neptune-triton is in some ways quite simple, as the parallelism
is mostly in the form of ``map``s, it did push the language hard in
one spot that had not seen much prior pressure.  That area was
something as superficially trivial as named constants.  That is,
top-level definitions that are not functions.  I `wrote about the
issue when we fixed it
<2020-07-07-futhark-0.16.1-released.html#significantly-better-handling-of-constants>`_,
so I will not repeat myself here, but I am still a little embarrassed
that the compiler had room to improve its compile time (for some
programs) by almost two orders of magnitude.

Inlining
--------

Neptune-triton is not a very large program by most standards, but the
Futhark compiler uses a compilation strategy that makes sure every
program gets a chance to make it big!  Specifically, Futhark
aggressively `inlines
<https://en.wikipedia.org/wiki/Inline_expansion>`_ almost every
function at every point it is used.  This is not because function
calls are expensive (they are not), but because inlining is perhaps
the most fundamental *enabler* of other optimisations.  You cannot do
`loop fusion <https://en.wikipedia.org/wiki/Loop_fission_and_fusion>`_
if the two loops are separated by a function call!  Normal compilers
spend a lot of time and effort on developing heuristics for which
functions to inline, in order to balance code growth and optimisation
opportunities, but in Futhark we take the easy way out.  This works
surprisingly well in practice, because Futhark programs tend to be
small, and in particular functions that are called in many places tend
to be small.  Unfortunately, neptune-triton is not like most Futhark
programs.

Fundamentally, neptune-triton is about performing operations on the
elements of finite fields.  Unsurprisingly, this means that for
example `the addition function
<https://github.com/filecoin-project/fut-ff/blob/801985c67955a9baf29a34128f548e0eb7a9a1e5/lib/github.com/filecoin-project/fut-ff/field.fut#L484>`_
is frequently called from *many* different locations.  Due to the use
of static vectors, each of these calls result in an inlined unrolled
loop.  This makes the generated code quite large.  And there is not
even much profit to be had: inlining those arithmetic code sequences
does not expose any opportunities for optimisation.  While
neptune-triton's heavy pressure on the inliner did motivate some
compiler improvements that ultimately benefited all programs (mainly
parallelisation and interleaving of simplification with inlining), the
right solution is clearly to improve our heuristics for inlining.  Via
manual use of the ``#[noinline]`` attribute, I have verified that this
could substantially cut compile times.

In theory, large generated GPU kernels could also have a negative
performance impact at run-time.  In practice, it appears that GPUs are
not particularly sensitive to program size - or at least, the kernels
will have to be much larger than even what Futhark generates.
However, large GPU kernels do still take a while to load, as this
requires a form of JIT compilation, which in practice manifests as
long program startup times.  OpenCL (and CUDA) does allow one to
extract the final machine-specific compiled GPU kernel, which can be
used to speed up subsequent loads substantially, but Futhark does not
make this usage very convenient.  This is definitely something we
should look at making more accessible.

Also, some low-quality OpenCL implementations (particularly the one
Apple uses in macOS) can crash on large programs, further motivating
the generation of smaller programs.

Integration
-----------

Futhark was never intended for writing full applications. Rather, it
was always intended that Futhark programs would be invoked by programs
written in other languages through a low-level C API.  While we did
`design and implement that API
<https://futhark.readthedocs.io/en/latest/c-api.html>`_ and used it
for `toy purposes <https://github.com/diku-dk/lys>`_, neptune-triton
was the first time it was really put through its paces.  Fortunately,
it held up well!  The choice to design a very low-level API, while
verbose, meant that we had little implicit or automatic behaviour that
could cause subtle integration issues.  Indeed, the main thing
neptune-triton accomplished was to encourage us to actually write down
full and accurate documentation for the API, which had until then been
documented via scattered examples and blog posts.

One interesting aspect of neptune-triton is that it is intended to be
made available as a Rust library.  To bridge the gap from Rust to the
C code generated by Futhark, neptune-triton makes use of `genfut
<https://github.com/Erk-/genfut>`_, a Rust-Futhark bridge developed by
`Valdemar Erk <https://github.com/Erk->`_.  This did require some
improvements to genfut, but I was not directly involved, and they did
not require any compiler modifications - a nice affirmation of the
usability of our C API.

Security audit
--------------

After neptune-triton became (supposedly!) correct and (demonstrably!)
fast, I was asked whether I would be interested in doing a paid
security audit of the implementation.  I was initially hesitant, as
security audits are best done by third parties with no prior interest
or involvement.  But I was convinced to agree after it was clarified
that the audit concerned not the Futhark compiler itself, nor the
cryptographic primitives in neptune-triton, but rather whether
neptune-triton itself made correct use of Futhark and its API.  That
sounded interesting, in particular because it would be an opportunity
to think systematically about what it *means* to use Futhark
correctly, and to document it for future use.  `The final report is
publicly available
<https://github.com/filecoin-project/neptune-triton/blob/e1ebadc5643dccad9c05d08ea634975a6805886e/neptune-triton-security-audit-report.pdf>`_
and I believe it is a good starting point for auditing other Futhark
programs.

One of the most challenging aspects of writing the report was judging
the risk of compiler bugs.  Any nontrivial compiler is going to have
miscompilation bugs (with `rare <https://cakeml.org/>`_ `exceptions
<http://compcert.inria.fr/>`_), and as Futhark is a pretty complicated
compiler with relatively few users, the risk is significant.  However,
I managed to identify two compensating factors:

* Poseidon distributes Futhark compiler *output* (C code), so only
  correct computation of a *specific program* with a *specific
  compiler* had to be shown.  Since neptune-triton is an
  implementation of a known algorithm, we could do this empirically by
  showing that it computed the same result as a reference
  implementation in Rust.

* Futhark performs full type checking after every compiler pass.  The
  vast majority of compiler bugs are caught by these checks rather
  than silently producing wrong code.

Finally, as Futhark is an *actually pure* language (with no escape
hatches like Haskell), any malfunction in a Futhark program is
realistically going to be limited to corrupting memory or computing
incorrect results - it will not mistakenly files from the disk or
anything of the sort.

In summary
----------

Optimising compilers are big bags of tricks and heuristics.  When
mature, it looks like they take a holistic view of compilation, but
really the bag has just become so full that it is hard to understand
what is happening anymore.  This works because most programs are
actually quite similar with respect to their optimisation needs.
Futhark is not yet this mature, so sometimes we get a program, like
neptune-triton, that jut happens to hit multiple weak points at once.
But now those points have been fixed, and while it was fun and
interesting, I hope it will be a while before we encounter a program
of this kind again.
