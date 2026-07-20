---
title: Rewriting the Futhark type checker
description: A post about how the Futhark has evolved over time and some information on the most recent large reorganisation of its machinery.
---

This post is about the evolution of Futhark's type checker, motivated by a
[large refactoring I am about to
merge](https://github.com/diku-dk/futhark/pull/2302). It is probably mostly of
interest to other language designers, and contains some lessons I wish I had
known when we first got started - although I am not particularly well-read in
the type checking literature, so it's possible all of this is old hat.

## Background

Far back [in the primordial
era](2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html)
when we first designed Futhark, the type system was simple: The only types were
scalars, arrays, and tuples, all functions were first-order and monomorphic, and
there was no type inference. All functions had to be annotated with their
parameter and return types. The only fancy feature that was present from the
(near) beginning was [in-place updates via a kind of uniqueness
types](2022-06-13-uniqueness-types.html), although at the time we didn't really
understand how tricky they were.

[At that
point](https://github.com/diku-dk/futhark/blob/48ca2dedf23990e2b0ffd00a26ab32ca4f0ef22c/src/L0/TypeChecker.hs)
it really was just a type *checker*: starting from the facts that we were told
(the types of parameters), we checked if the types of expressions were
consistent, and tracked aliases to validate the use of in-place updates. The
implementation was a straightforward top-down single traversal of each function
in turn, during which AST were decorated nodes with their determined type, which
was then used by subsequent passes. It was the exact same kind of type checker
that I had been taught (and since taught myself) in a standard course on
programming language implementation, and when you can get away with it, this is
a *good* design: easy to implement, pretty efficient, easy to understand. At the
time we really didn't understand the complexity of *correctly* checking the
rules for safe use of in-place updates, so the simplicity was perhaps illusory,
but we were happy in those days nonetheless.

Of course, we didn't get away with it for long. We eventually started adding
features such as [records](2017-03-06-futhark-record-system.html) and an
[ML-style module system](2017-01-25-futhark-module-system.html), and by version
0.4.0 [we had put in higher-order functions and Hindley-Milner style type
inference](2018-04-10-futhark-0.4.0-released.html). The implementation was a
classic (some might say "old-fashioned") implementation of [Algorithm
W](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html);
the standard Hindley-Milner type inference algorithm. During traversal of a
function, we immediately solved type constraints. However, we had some type
system features that were never a natural fit with this approach. Both our
record system, and also our uniqueness types, depended on being able to query
the current type of some program fragment and make decisions based on it. This
would show up in cases like branches, where the aliases of `if a then b else c`
depended on whether the type of `b` and `c` can carry aliases (primitives
cannot). We managed to put together an implementation that worked by putting
various extra information into our representation of type variables, but it was
a little complicated, not obviously correct, and somewhat tedious to debug.

Later, when we added [proper size types](2019-08-03-towards-size-types.html), we
did it by extending the basic Hindley-Milner system by essentially also treating
sizes as a kind of "type" subject to unification. At this point the system was
beginning to creak. Consider again a branch `if a then b else c` - what is the
size of the result? If `b` and `c` are arrays, we need to determine whether they
have the same size, and if not, generate an existential for the result size. But
at the point where we are looking at this `if`, we may not yet know whether they
are arrays! Many similar cases occurred, and the type system had various
restrictions and quirks where the well-typedness of a function would depend in
somewhat subtle ways on whether you had put in type annotations. One nice
property however was that if you put in type annotations on all parameters,
which is generally considered good style anyway, then things worked pretty
predictably. But still, the type checker was not particularly fun to hack on at
this point.

Eventually, this design - a single program traversal manipulating an
increasingly complicated state - stopped being workable. This was when we
started working on [AUTOMAP](2024-06-17-automap.html), a (not yet finished)
system that loosely means that instead of saying `map f x`, you can just say `f
x` and have the compiler figure out how many `map`s are needed. During this
work, we discovered that AUTOMAP had to work by taking a global view of a
function, typechecking it partially but without inferring specific sizes of
arrays, determining the smallest number of `map`s needed to make it typecheck
(by using an ILP solver), and then determine specific sizes. There was no way to
fit this design into the existing type checker, so I began taking it apart into
multiple phases.

The first phase I extracted was [name
resolution](https://github.com/diku-dk/futhark/blob/master/src/Language/Futhark/TypeChecker/Names.hs).
It does what you would expect: resolves every name in the program, in practice
by assigning each a unique identifier. This was a small win, but it simplified
the type checker a little. The next phase was the conceptually final one:
[checking for aliases and in-place update
violations](https://github.com/diku-dk/futhark/blob/master/src/Language/Futhark/TypeChecker/Consumption.hs).
This turned out to be *much* more profitable, as this machinery had always been
hindered by having to work with incomplete type information. Now it would be
given a *fully well-typed program* and simply had to figure out whether any of
the in-place constraints were violated (essentially, whether in-place updates
are semantically observable).

We were now left with a remaining core piece: a "term type checker" that did
both normal ML-style type inference, but also size inference. As an expediency,
to make the AUTOMAP research project progress, I did a hacky thing: I wrote an
entirely new *constraint-based* type checker that did not perform size
inference, but rather only standard type inference. We call this the *unsized
type checker*. Instead of solving type equations on the fly, it generated a set
of *type constraints* (mostly type equalities), which were solved "offline" by a
constraint solver. This "generate-and-solve" approach makes handling complicated
type system features more tractable as you don't have to make decisions based on
partial information, and is [also used by the Glasgow Haskell
Compiler](https://www.youtube.com/watch?v=OISat1b2-4k) and the
[Flix](https://flix.dev/). In our implementation, we used these constraints to
drive the AUTOMAP solver, then *threw away those constraints and re-ran the
original size-aware type checker* (along with a bit of extra information
provided by AUTOMAP). This *worked*, in that all programs could type check, but
I think [our paper](https://futhark-lang.org/publications/oopsla24.pdf) puts it
generously when it says "our Automap-enabled type checker is unoptimized". My
concern was not merely the inefficiency of redundant work, but the lack of
clarity by *duplicated logic* - every type checking rule was duplicated in code!

## The new type checker

I still have some hope for AUTOMAP itself, but getting it in a workable state
requires an unknown amount of implementation effort. However, during this work I
had become attracted to the elegance of constraint-based type checking. The
central advantage is that you can postpone decisions until all information is
available. From a human perspective, it is also *much easier* to debug (or
benchmark) when you can print out all the constraints and reason about them.
Unfortunately, our implementation was abominable. Last year I [started work on
doing constraint-based type checking without
AUTOMAP](https://github.com/diku-dk/futhark/pull/2302), and in particular where
duplication of logic was minimised. The idea was to use the unsized type checker
to infer "ground" types (meaning types without filling in the specific sizes of
arrays), then do a second pass that performs *only* size inference.

The main obstacle, and the reason I didn't finish until now, stemmed from tricky
cases related to existential sizes, where existential quantifiers of array sizes
must be inserted in just the right place. Consider a function with size-lifted
type variables like this:

```Futhark
def apply 'a '^b (f: a -> b) (x: a) =
  f x
```

Since `^b` is size-lifted, it can be instantiated with a type that contains
existential quantifiers, in this case meaning that `f` can be a function that
returns an array of an unknown size.

If we say `apply iota`, then the unsized type checker will infer this type for
`apply`:

```Futhark
(f: i64 -> []i64) -> (x: i64) -> []i64 =

```

When we perform size inference, it is quite important that we infer this type,
with existential quantification (`?[n]`) in return types:

```Futhark
(f: i64 -> ?[n].[n]i64) -> (x: i64) -> ?[m].[m]i64
```

Rather than let the return size float all the way to the top as a size
parameter:

```Futhark
(f: i64 -> [n]i64) -> (x: i64) -> [n]i64
```

This incorrect type states that the array returned by `f` is known before `f` is
invoked at all, and does not depend on its parameter. This is true for *some*
`f`s, but not when `f=iota`.

In the old type checker, this was done with some clever (oh no...) programming
deep in the middle of unification whenever it encountered flexible size-lifted
type variables. Since the unsized type checker has now determined all ground
types in advance, no flexible type variables are *ever* encountered, so I had to
work out a different solution. The details are too intricate to get into here,
but they are ultimately a more explicit handling of the issue. There are still
*some* ugly parts where I think we never worked out a good theory for size
inference, but the part of the code that they infect is now a lot smaller than
it used to be. I still have a vague idea that we should redo all of size
checking at one point, perhaps basing it on [bidirectional type
checking](https://ncatlab.org/nlab/show/bidirectional+typechecking) instead of
full inference, since it really is a dependent type system.

But that is for the future. With this work, the Futhark type checker now goes
through four stages for each function:

1. Resolve names.
2. Determine ground types with the unsized type checker.
3. Perform size inference.
4. Check for in-place update violations.

(If we ever finish AUTOMAP, it will go between stages 2 and 3.)

One lovely property of this design is that if an error is encountered in stage
*i*, we have full information from stage *i-1* to help explain the error. This
is a nice change from the original single-pass type checker, where if something
went wrong, we would usually have *no* information about those parts of the
program that we have yet to traverse. I suspect a general principle that it's a
good idea to design a type system in "layers", with each adding more
restrictions to the former, such that a program's meaning can be understood even
when it is not fully type-correct at all layers.

I also found it useful to put as many checks as possible into a separate pass
that runs after the main type checking. For example, Futhark has restrictions on
higher-order functions: you cannot return them from branches or loops.
Previously, we would decorate pertinent type variables with such information,
which complicated the core inference algorithm and sometimes resulted in
hard-to-understand error messages. Now, we run through the program after type
inference and look for any such forbidden uses. If any are found, it is easy to
pinpoint the cause: you tell the user that *this* expression is not allowed, and
*this* is the type we inferred for it.

One potential downside of this approach is inefficiency - it is usually more
expensive to do four things than one thing. Further, some of these stages
technically do more than one traversal of the function (in particular size
inference) to check for various other problems or update AST decorations.

Now for an aside on compiler efficiency. Futhark was never intended to compile
quickly. Personally, when I first started working on Futhark, I was inspired by
such projects as [Stalin](https://github.com/barak/stalin) and
[MLton](http://www.mlton.org/), which were famous for combining unexpected high
performance with very long compile times. My aspiration was for people to say of
Futhark "it sure can generate fast code if you wait for it". Perhaps I even
intended to take a bit of pride in compile times being as impractical as Stalin,
and it would certainly serve as a convenient excuse for why the language wasn't
used much. (Although while MLton is slow as compilers go, hardware improvements
have made it quite practical these days.) However, since then Futhark has not
only been used by (a few) other people; we also use it ourselves to conduct
research into [parallel programming
techniques](2026-06-18-data-parallel-pretty-printing.html). It is a lot more fun
to do this when you can get reasonably prompt feedback from the compiler, and
hence while the Futhark type checker is certainly by most standards quite slow,
we *do* try to some extent to not make it much slower than it currently is.

In my initial work on splitting the type checker, I observed a 20% slowdown of
`futhark check` on [a representative
program](https://github.com/diku-dk/futhark-benchmarks/blob/master/finpar/LocVolCalib.fut),
but I managed to get that down to about -4% (that is, a *speedup*) through
various optimisations. Most of these were tedious tricks: checking whether some
work was truly necessary before undertaking it. Could I have added such
optimisations to the old type checker as well and thus made it even faster? Yes,
but I didn't. I'm not trying to make the fastest type checker, merely to not
make it even slower. (But if someone wants to write fast type checker, please do
reach out! I think the current design makes it fairly easy to benchmark
components in isolation.)

As part of of this work, I also prepared the type checker to allow recursion
(despite [previous](2016-09-03-language-design.html)
[concerns](2026-01-20-why-not-tail-recursion.html)), currently gated by the
secret environment variable `FUTHARK_ALLOW_RECURSION`, but that is a story for a
different day.
