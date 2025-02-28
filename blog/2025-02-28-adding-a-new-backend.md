---
title: What it takes to add a new backend to Futhark
description: A reader asked and the Futhark devblog answers the call.
---

Recently Scott Pakin [suggested writing a blog post on how to add a
new backend to the Futhark
compiler](https://github.com/diku-dk/futhark/discussions/2224), and
since there's active fiddling with the backends at this very moment,
this is not a bad idea. Let us manage expectations up front: this will
not be a *tutorial* on adding a backend. I will not go into the deep
details on the specific internal APIs that should be used. Instead, I
will focused on the core representations, and give an idea about the
kind of work (often complicated) and magnitude (sometimes relatively
little) it takes to add a new backend. It's also somewhat open
precisely what a "backend" even means. There's a significant
difference in complexity between adding a command `futhark foo
bar.fut` that produces *something* based on `bar.fut` (very easy), to
implementing another C-like GPU backend (not hard, but you need to
touch a lot of pieces), to creating a fundamentally new backend for an
alien piece of hardware (depending on your needs, can be extremely
challenging).

I will still link pertinent pieces of the source code as applicable -
sometimes it is instructive just how simple (or simplistic) it is to
finally glue together the complex bits. The Futhark compiler currently
supports a fairly diverse set of targets (sequential CPU, multicore,
different GPU APIs, C, Python). To achieve this without undue
duplication of code and effort, the compiler uses fairly heavily
parameterised representations of the program being compiled. I'll try
to get the gist across, but the full details are very, well, detailed
(and I always feel like they should be simplified - it's not the
aspect of the compiler we're most proud of).

For a drier exposition, there is also [the internal compiler
documentation](https://hackage.haskell.org/package/futhark-0.25.27/docs/Futhark.html).

## Architectural overview

The Futhark compiler is a monolithic program written in Haskell. All
passes and backends are part of the same executable. In principle, it
would not be too difficult to make it possible to write a backend in a
different language, as a separate executable, although it hasn't been
relevant so far.

The compiler consists of three main parts:

  * The *frontend*, which is concerned with parsing the Futhark source
    language, type-checking it, and transforming it to the core
    intermediate representation (IR).

  * The *middle-end*, which performs gradual refinement,
    transformation, and optimisation, on a program represented in
    various variants of the the IR format (more on that below).

  * The *backend*, which translates from the IR representation into
    some lower level representation, such as C - likely via several
    steps.

The frontend is pretty much the same no matter how you invoke the
Futhark compiler (`futhark c`, `futhark cuda`, etc), but the
middle-end and backend behaves very differently based on the compiler
mode. For example, rather than having a single IR, the compiler
actually has a *family* of IR dialects, suited for different purposes,
and used at different stages of the compiler. To give a gist of what I
mean, consider an extensible Haskell datatype for representing very
simple expressions:

```Haskell
data Exp o = Var String
           | Val Int
           | Add (Exp o) (Exp o)
           | Sub (Exp o) (Exp o)
           | Op o
```

Apart from representing variables, values, additions, and
subtractions, this `Exp` type has also has a type parameter `o` that
represents some *other* kind of operation, via the `Op` constructor.
This means we can instantiate a variant of `Exp` that contains, say,
square root as the operation:

```Haskell
data SqrtOp = Sqrt ExpWithSqrt

type ExpWithSqrt = Exp SqrtOp
```

But we could also have one with some general notion of a function call:

```Haskell
data FunOp = Fun String [ExpWithFun]

type ExpWithFun = Exp FunOp
```

We can now write functions that operate on `Exp o` values, as long as
we parameterise those functions with how to handle the `o` cases
(using function parameters, type classes, or what have you). This
technique is useful when we want to have a collection of types that
are largely the same. For example, in the middle-end of the Futhark
compiler, we initially use an IR dialect called `SOACS`, where
parallelism is expressed using nested higher order operations quite
similar to the source language. Eventually, the program undergoes a
[flattening transformation](2019-02-18-futhark-at-ppopp.html), after
which parallelism is expressed using a different vocabulary of
flat-parallel operations. Most of the language-level details of the
IR, such as how arithmetic and control flow is expressed, remains the
same, and so does a lot of compiler code, such as simplification
passes, that can operate on any dialect.

The actual representation is a little more involved than explained
above, and is quite similar to the approach described in the paper
[Trees that
Grow](https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/trees-that-grow.pdf).

We use this notion of IR dialects pervasively throughout both the
middle-end and the backend.

## The backend

