---
title: Tracking source locations
description: How the Futhark compiler got a little better at blaming the programmer for their programs being slow.
---

Futhark is a programming language meant for writing fast programs, but as is the
case for every programming language meant for writing fast programs, it
inevitably happens that a programmer will use it to write a program that is not
fast. When this happens, the programmer will likely want to know why their
program is not fast, and how to make it faster. A useful tool for answering such
questions is a *profiler* - a tool that tells you how long the different parts
of your program take to run. This post is about how profiling in Futhark became
slightly more useful with the [most recent
release](https://github.com/diku-dk/futhark/releases/tag/v0.25.32).

Initially, Futhark had no real profiling support, except for some
semi-documented support for dumping a report of GPU operations. Eventually we
added [`futhark
profile`](https://futhark.readthedocs.io/en/latest/man/futhark-profile.html),
which allows the machine-readable profiling data produced by [`futhark
bench`](https://futhark.readthedocs.io/en/latest/man/futhark-bench.html) to be
turned into human-readable reports. Specifically, the Futhark runtime system
will tally up the time spent in various cost centres, which for the GPU backends
are GPU kernels and other operations such as copies, and put it in a table.
However, the information you get out still looks like this:

```
                         Cost centre           count             sum             avg             min             max        fraction
------------------------------------------------------------------------------------------------------------------------------------
builtin#replicate_i8.replicate_25280              63        346.11μs          5.49μs          5.12μs          7.17μs          0.0019
copy_dev_to_dev                                    6         80.90μs         13.48μs          8.19μs         27.65μs          0.0004
copy_lmad_dev_to_dev                               1          8.19μs          8.19μs          8.19μs          8.19μs          0.0000
initOperator_6347.gpuseq_25229                     2         20.48μs         10.24μs          9.22μs         11.26μs          0.0001
initOperator_6347.replicate_25196                  2         22.53μs         11.26μs         11.26μs         11.26μs          0.0001
initOperator_6347.segmap_19237                     2         31.74μs         15.87μs         14.34μs         17.41μs          0.0002
main.gpuseq_26401                                 63        382.98μs          6.08μs          5.12μs          7.17μs          0.0021
main.gpuseq_26416                                 63        344.06μs          5.46μs          5.12μs          7.17μs          0.0019
main.replicate_25225                               1        184.32μs        184.32μs        184.32μs        184.32μs          0.0010
main.segmap_19296                                  1         23.55μs         23.55μs         23.55μs         23.55μs          0.0001
main.segmap_19320                                  1         13.31μs         13.31μs         13.31μs         13.31μs          0.0001
main.segmap_23375                                  1         13.31μs         13.31μs         13.31μs         13.31μs          0.0001
main.segmap_23407                                 63        408.58μs          6.49μs          6.14μs          7.17μs          0.0022
main.segmap_23494                                 63        421.89μs          6.70μs          6.14μs          8.19μs          0.0023
main.segmap_23553                                 63       9211.90μs        146.22μs        144.38μs        147.46μs          0.0501
main.segmap_intrablock_23610                      63     100120.57μs       1589.22μs       1573.89μs       1616.90μs          0.5445
main.segmap_intrablock_24377                      63      60806.14μs        965.18μs        955.39μs        985.09μs          0.3307
main.segscan_23448                                63        785.41μs         12.47μs         11.26μs         13.31μs          0.0043
map_transpose_4b                                 127      10655.74μs         83.90μs         80.90μs         89.09μs          0.0579
------------------------------------------------------------------------------------------------------------------------------------
```

Now a user may reasonable object: *"Hold on! I don't remember [my
program](https://github.com/diku-dk/futhark-benchmarks/blob/master/finpar/LocVolCalib.fut)
containing anything called `main.segmap_23494`!"* And indeed, these cost centres
refer to compiler-generated names. You can squint to get *some* meaning out of
them: `segscan` is certainly some kind of `scan` operation, and `segmap` is a
`map`. But due to inlining, it can be difficult to guess which functions result
in which GPU operations, and optimisations may obscure the relation between
source code and generated code - indeed, those `segmap_intrablock` operations
are actually mainly (nested) `scan`s that are then turned into block-level scans
via [incremental
flattening](https://futhark-lang.org/blog/2019-02-18-futhark-at-ppopp.html). But
clearly it is still not easy to use this information. The profiler will usually
just tell the programmer that their program spends all its time executing code
with a name the programmer cannot possibly recognise. What is missing is a way
to relate generated code with the original source code. I decided to call such
information *provenance*, in the sense of "the ultimate origin of something".
The problem is then to attach provenance to every bit of generated code, and in
particular, to the generated GPU kernels.

Unfortunately, the Futhark compiler was never really designed to track
provenance. The frontend always annotated all syntactic constructs with precise
source information in order to do good error reporting, but it was thrown away
during desugaring and lowering into the intermediate language (IR). To improve
the usability of profiling, we had to actually propagate source information
throughout the entire compiler - and I was quite uncertain about how practical
it would be to make such a change to a [ten year old code
base](https://futhark-lang.org/blog/2021-12-19-past-and-present.html) comprising
over a hundred thousand source lines of Haskell. The main challenge is that the
Futhark compiler does a *lot* of optimisations where it rewrites part of the
program. Manually propagating provenance is simply not viable or maintainable.
We had to come up with some kind of semi-automated general scheme.

Further, there were some conceptual questions about how to track provenance
across the compiler, due to the aggressive transformations it does. For example,
the user may write something like this:

```Futhark
let b = map f a
let c = map g b
let d = map h c
```

The compiler may then perform fusion to essentially turn the program into this:

```Futhark
let d = map (\x -> h (g (f x))) a
```

Now what is the provenance of the resulting `map`? A reasonable answer is that
it is simply the union of the three original lines of code. (Let us ignore the
issue of what that looks like if those lines are not actually close to each
other.) But the compiler may then later perform *fission* to split up the `map`
again, if it decides that is the best way to exploit the parallelism in the
mapped functions:


```Futhark
let tmp = map (\x -> g (f x)) a
let d = map h tmp
```

Now what is the provenance of the resulting `map`s? If we already merged
together the provenances of the original three `map`s into the single fused one,
then we have lost some of the original information, and our only choice is to
consider that *both* of the two final `map`s originate in the three initial
`map`s. In practice, the functions `f`, `g`, and `h` are also not black boxes,
and may be intermixed with each other before being split apart.

This is not an edge case: the basic compilation scheme of the Futhark compiler
is to fuse as much as possible based on data dependency information in order to
achieve some kind of "normal form" representation, then *statically schedule*
the resulting parallel nests via various flattening transformations, based on
its understanding of the concrete parallel hardware (GPUs, for the purpose of
this post). Naive tracking of provenance thus runs the risk of concluding that
every part of the output depends on every part of the input. This is perhaps
true in a philosophical sense, but it is not very useful to a programmer.

So this is the dilemma: we want something we don't have to think about too much,
but it also has to be quite precise.

## What now

The solution to the first problem is to piggyback on an already existing
facility for attaching information to IR statements, which we currently use for
[certificates](https://futhark-lang.org/blog/2023-03-17-on-nontermination.html).
Essentially, each Futhark statement is associated with a set of certificates
that function as fake data dependencies, which ensures that safety checks are
always executed *before* the expression they protect. As this is
safety-critical, we have fairly high confidence that the compiler propagates
these correctly (because we fixed lots of bugs where it didn't). The way it
works is that each statement is associated with a
[`StmAux`](https://github.com/diku-dk/futhark/blob/fbe726ee74825898e341e644c9df7a5c76bad37f/src/Futhark/IR/Syntax.hs#L204-L210)
structure, and whenever you write code that changes, merges, or splits some
statement, you use a monadic facility for "propagating" the `StmAux` of the
dependencies. In many cases, code that transforms a single statement will just
reuse the original `StmAux`. By adding a provenance field to the `StmAux`, we
could simply piggyback on all the existing code, as long as we defined a
mechanism for merging two different provenances. This latter part remains a bit
crude, and simply takes the union of source locations (with sometimes odd
results of the source locations are from different files - but this can always
be refined later, as it is [well separated from everything
else](https://github.com/diku-dk/futhark/blob/fbe726ee74825898e341e644c9df7a5c76bad37f/src/Futhark/IR/Syntax/Core.hs#L679-L684)).

The second problem, when we cut apart `map`s, is a bit more tricky. Our solution
was a bit of a convenient hack, but it has proven surprisingly accurate and
useful in practice. The idea is to disregard the provenance of the `map`s
themselves when deciding on the provenance of a generated GPU kernel, but *only
look at the provenances of the leaf scalar operations inside of them*. Following
up on the example above, it means that the provenance of the first `map` will be
the union of the provenance of `g` and `f`, and the provenance of the second
`map` will be the provenance of `h`. This has the slightly odd consequence that
the source location of a GPU kernel never points to a `map`, but to the function
(often a lambda) that it is passed, although this does sort of make sense in a
way - the `map` itself is not the cost centre; it is just control flow. It is
the stuff inside of it that is the actual computation.

There's a little bit more to it - the provenance representation is not just a
single source location, but can also embed a bit of a (static) stack trace, to
properly track the case where the same code is inlined in multiple locations.
After these changes, the timeline log produced by `futhark profile` now ends
like this:

```
...
main.gpuseq_26416
Duration: 5.12 μs
At: LocVolCalib.fut:187:16-90->LocVolCalib.fut:175:33-177:69->LocVolCalib.fut:162:20-66->LocVolCalib.fut:140:14-41->LocVolCalib.fut:100:21-27
Kernel main.gpuseq_26416 with
  grid=(1,1,1)
  block=(1,1,1)
  shared memory=0

main.segmap_intrablock_24377
Duration: 956.416016 μs
At: LocVolCalib.fut:184:17-188:12
Kernel main.segmap_intrablock_24377 with
  grid=(65536,1,1)
  block=(256,1,1)
  shared memory=3072

map_transpose_4b
Duration: 81.919998 μs
At: LocVolCalib.fut:187:16-90->LocVolCalib.fut:175:33-177:69->LocVolCalib.fut:163:6-26
Kernel map_transpose_4b with
  grid=(8,8,256)
  block=(32,4,1)
  shared memory=4224

copy_lmad_dev_to_dev
Duration: 8.192 μs
At: LocVolCalib.fut:184:17-188:12
Kernel copy_lmad_dev_to_dev with
  grid=(1,1,1)
  block=(256,1,1)
  shared memory=0
```

This is perhaps not the ideal way of reporting the information, but it shows
that it actually makes it all the way through the compiler, and can be used in
its current form by programmers to understand how to speed up their Futhark
programs. (Human trials still pending, since all the students here are on summer
vacation.)

## What next

The plumbing for tracking provenance now works, which was the part I was most
worried about. There are probably places where the compiler propagates
provenances in a less-than-useful way (or drops them entirely), which we will
address on a case-by-case basis. But the remaining steps are about improving the
usability of how the profiling information is presented.

One rather obvious problem is that the provenances are not actually made
available in a particularly machine-readable way - they ultimately end up in a
timeline log file as above. There is also not any convenient way to associate
the table of cost centres, shown at the beginning of this post, with source
locations, except by searching for the cost centres in the timeline log. These
things are not so difficult to improve - it is just a matter of writing some
code for data processing, which I am informed that computers are good at.

I think it is also possible to create a nice heat map of the program source code
based on the profiling information, where hot sections of code will show up as
red (or whichever colour the
[HCI](https://en.wikipedia.org/wiki/Human%E2%80%93computer_interaction) people
say is associated with speed - maybe blue?). This may be more approachable than
reading long text files.

One more tricky problem is that although we track provenance at a fairly
fine-grained level of individual scalar operations, the Futhark runtime system
only tracks the runtime at the granularity of GPU operations. This means that
the profiler can tell you that some particular kernel is slow, and also which
span of source code that kernel corresponds to, but it cannot tell you which
*part* of that source is the actual slow part. Fixing *that* is unfortunately a
lot more tricky - we would somehow have to associate source locations with the
actual GPU kernel code we generate, and then also make use of the profiling
support of the underlying GPU platform API. I know that NVIDIA's profiler is
pretty good, so I think this is technically possible, but it will require
significant engineering effort in our kernel code generator.

One very cool thing would be if the profiler could also make suggestions for why
a kernel may be slow, phrased in the terminology of the Futhark *language*,
rather than GPU jargon. In particular, one very common reason for why Futhark
programs are slow is simply that there is not enough parallelism. It would not
be too difficult for the profiler to detect that some very slow kernel consists
of very few threads, and suggest that perhaps the program is too sequential.
(Suggesting how to parallelise a sequential algorithm is perhaps too ambitious.)

I also need to take a more systematic look at how other compilers handle this
problem. It maybe be that I spend a couple of weeks to save myself a day of
reading.
