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
    various dialects of the the IR format (more on that below).

  * The *backend*, which translates from the IR representation into
    some lower level representation, such as C - likely via several
    steps.

These parts form a chain. The compiler will always run the frontend,
ultimately producing an intermediate representation of the program,
then run an appropriate middle-end *pipeline*, which produces another
representation of the program, and finally pass this to the backend.

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
flat-parallel operations. Later, even the representation of types goes
from something similar to the source language, to a representation
that also contains information about memory layout. Most of the
language-level details of the IR, such as how arithmetic and control
flow is expressed, remains the same, and so does a lot of compiler
code, such as simplification passes, that can operate on any dialect.

The actual representation is a little more involved than explained
above, and is quite similar to the approach described in the paper
[Trees that
Grow](https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/trees-that-grow.pdf).
In particular, type-level functions are used to avoid having a
different type parameter for everything that can vary.

We use this notion of IR dialects pervasively throughout both the
middle-end and the backend. The middle-end uses a *pipeline* based on
the compilation mode, which eventually produces a program in some IR
dialect. That is, pipelines can be considered pure functions from some
IR dialect to some other (or the same) IR dialect. For the `c`
backend, this dialect is called `SeqMem` (no parallelism, with
[information about array layout and
allocations](2024-03-06-array-representation.html)), for the
`multicore` and `ispc` backends it is `MCMem` (multicore parallel
operations), and for the GPU backends it is called `GPUMem`. You can
[see some of the default pipelines
here](https://github.com/diku-dk/futhark/blob/4f2d5e67c744bfef87e4410176e7d52e980603da/src/Futhark/Passes.hs).

Writing a new backend thus consists of picking a pipeline that
transforms the IR into the dialect you wish, and then doing
*something* with that IR - where the compiler is actually quite
agnostic regarding what that *something* might be. Not every backend
needs a distinct IR dialect - all of the GPU backends use the same IR
dialect, for example.

## Backend actions

In Futhark compiler implementation lingo, the "backend" is called an
"action", and is an essentially arbitrary procedure that runs on the
result of a middle-end pipeline:

```Haskell
data Action rep = Action
  { actionName :: String,
    actionDescription :: String,
    actionProcedure :: Prog rep -> FutharkM ()
  }
```

Here `rep` is a type-level token representing the IR dialect accepted
by the action, and `FutharkM` is monad that supports IO effects,
meaning that these "actions" can perform arbitrary IO. For example,
the action for `futhark c` will do a bunch of code generation in pure
Haskell code, but *also* write some files and run a C compiler:

```Haskell
compileCAction :: FutharkConfig -> CompilerMode -> FilePath -> Action SeqMem
compileCAction fcfg mode outpath =
  Action
    { actionName = "Compile to sequential C",
      actionDescription = "Compile to sequential C",
      actionProcedure = helper
    }
  where
    helper prog = do
      cprog <- handleWarnings fcfg $ SequentialC.compileProg versionString prog
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"

      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = SequentialC.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ SequentialC.asExecutable cprog
          runCC cpath outpath ["-O3", "-std=c99"] ["-lm"]
        ToServer -> do
          liftIO $ T.writeFile cpath $ SequentialC.asServer cprog
          runCC cpath outpath ["-O3", "-std=c99"] ["-lm"]
```

Here the `SequentialC.compileProg` function does the actual C code
generation. I'll elaborate a bit on it, but at an architectural level,
it is not constrained at all in what it does. In principle, an action
could just dump the final IR to disk and run some entirely different
program that takes care of code generation. You might even write an
action that expects the program to still be in one of the early IR
dialects, such as the ones that do not have memory information, or
even the one that still has nested parallelism. This might be
appropriate if you are targeting some other (relatively) high level
language.

Ultimately, if you wish to write a backend that does not need a new IR
dialect, and also does not need to reuse any of the existing C
generation machinery, then this is consequently quite easy - at least
as far as integration with the compiler is concerned.

To actually hook up a pipeline with an action and produce something
that can be invoked by the command line, you need to write a largely
boilerplate `main` definition, like [this one for `futhark
c`](https://github.com/diku-dk/futhark/blob/master/src/Futhark/CLI/C.hs):

```c
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile sequential C"
  "Generate sequential C code from optimised Futhark program."
  seqmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCAction fcfg mode outpath) prog
```

And then [finally hook it up to the big list of
subommands](https://github.com/diku-dk/futhark/blob/master/src/Futhark/CLI/Main.hs).
That's all it takes.

## Imperative code generation

While it is true that an action can be arbitrary imperative code, in
*practice* all of Futhark's C-based backends (and even the [Python
ones](https://futhark-lang.org/blog/2016-04-15-futhark-and-pyopencl.html))
make use of significant shared infastructure to avoid having to
reimplement the wheel too often.

As a starting point, the Futhark compiler defines an *imperative*
intermediate representation, called
[Imp](https://github.com/diku-dk/futhark/blob/master/src/Futhark/CodeGen/ImpCode.hs).
As with the middle-end, Imp is actually an extensible language, with
various dialects. For example, [sequential
ImpGen](https://github.com/diku-dk/futhark/blob/master/src/Futhark/CodeGen/ImpCode/Sequential.hs).
In contrast to the functional middle-end IR, which is very well
defined, with type checking rules and a well-defined external syntax,
Imp is a lot more ad hoc, and does not for example have a parser.
Semantically, it's largely a simplified form of C. In fact, it is not
even in [SSA
form](https://en.wikipedia.org/wiki/Static_single-assignment_form),
which still works out alright, because we do *no* optimisation at the
Imp level.

The translation from the functional IR to Imp is done by a module
called
[ImpGen](https://github.com/diku-dk/futhark/blob/master/src/Futhark/CodeGen/ImpGen.hs).
It is heavily parameterised, because it essentially has to go from an
*arbitrary IR dialect* to *an arbitrary Imp dialect*. It is full of
implementation details, but not particularly interesting.

Once the compiler has obtained an Imp representation of the program,
it can then turn that program into C or Python, or even some other
language. This is largely a mechanical process - the semantic gap from
Imp to C (or the baroque form of Python produced by Futhark) is not
great, and mostly involves mapping Imp constructs to the facilities
provided by the (very small) Futhark runtime system, and of course
generating syntactically valid code. To ease maintenance of the three
GPU backends (`cuda`, `opencl`, `hip`), we also make use of a small
GPU abstraction layer
([gpu.h](https://github.com/diku-dk/futhark/blob/master/rts/c/gpu.h),
discussed in [this
paper](https://futhark-lang.org/publications/fproper24.pdf).

## Advice on writing a backend

Futhark is not set up to make it especially easy to add new backends,
but neither is it particularly difficult. After all, as of this
writing we support 10 different backends. Here is some advice for any
prospective people who wish to seek glory by adding a backend:

* If you want to target a very high level parallel language, use only
  Futhark's frontend and the middle-end up to the `SOACS`
  representation. This will give you a monomorphic first-order program
  (except for parallel operations) where all types are scalars or
  arrays of scalars, but still with nested parallelism, although that
  parallelism will be well fused. I do think it would be a fun
  experiment to generate code for a fork-join language, such as
  [MPL](https://github.com/MPLLang/mpl), from this representation.

* If you want to target a slightly less high level parallel language,
  in particular one that does not handle nested parallelism well,
  consider processing the output of the `GPU` representation. Despite
  the name, it is not *truly* GPU specific (the parts that are can be
  ignored or modified, and are mostly about metadata and tuning), but
  merely guarantees the absence of nested parallelism. It is still
  high level and with value-oriented semantics, with no notion of
  memory.

* If you want to target a new GPU backend, implement the `gpu.h`
  abstraction layer. The code generation work for CPU-side work will
  then be fairly straightforward, although you may still need to do
  significant work to generate the actual GPU kernel code. We are
  currently going through this process with the in-progress [WebGPU
  backend](https://github.com/diku-dk/futhark/pull/2140), and most of
  the challenges are related to the particular limitations of WebGPU
  (a post for another time), and not so much the compiler engineering.

* If you want to generate low level code of any kind, you will likely
  find it easiest to use one of the IR dialects with memory
  information. If you want to generate something that is relatively
  C-like (and generating e.g. machine code or JavaScript is both
  "C-like" in this regard), then using the existing machinery for
  generating Imp is almost certainly easiest.

However, in all cases, I would say a very good idea is to contact one
of the Futhark developers for advice and help. Having a third party
add a new backend is not really something we have considered much (all
of the backends have been written under our close supervision), and
while the *technical* challenges are not all that major by the
standards of writing compiler backends, the *documentation* is not
really up to the task. But I would certainly be very excited for
someone to give it a try.
