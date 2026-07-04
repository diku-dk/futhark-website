---
title: The cost of constants
description: While assigning a cost model to constants in a programming language isn't exceedingly complicated, there are still some quirks that are interesting to think about.
---

Futhark is a high level language ([except when it
isn't](2022-04-04-futhark-is-a-low-level-language.html)), and one of the ways in
which this manifests is through the availability of a highly optimising compiler
that may [radically restructure programs](2019-02-18-futhark-at-ppopp.html). One
of the challenges caused by such compilers is that programmers may not have a
clear idea of what makes a program efficient - if the compiler rewrites the
program, how are we supposed to reason about whether our code is good or not? We
can of course benchmark the resulting code and observe the consequences of
changes we make, but this is neither a particularly pleasant way of working, nor
is it effective when writing library code.

In Futhark, our solution since the beginning has been to define a language-based
cost model that specifies asymptotic runtime costs for the various expressions
and functions. It is essentially a more precise form of the [big-O
analysis](https://en.wikipedia.org/wiki/Big_O_notation) that most programmers
know from conventional studies of algorithms. The cost model then provides us
with a way of reasoning abstractly about the performance of our program, and
[forms the basis for a contract between the programmer and the
implementation](2022-01-27-cost-models-are-contracts.html) - the program must
behave at least as well as promised by the cost model. Futhark even has a cost
model that distinguishes *work*: the total number of operations; from *span*:
the longest chain of sequential dependencies. This is obviously important for a
parallel language, but is actually orthogonal to the issue at hand in this blog
post. Instead I will ruminate on how these cost models interact with top level
constants, how seemingly benign optimisations may have undesirable consequences,
and how maybe sometimes it takes a lot of effort to avoid breaking promises that
the recipient may not actually care about.

One of Futhark's least unusual features is that it supports constant value
bindings at top level. They look like this:

```futhark
def physicists_pi = 4.0
```

They behave semantically as you would expect: when they are encountered, they
are fully evaluated to a value. The one above is a literal, but value bindings
can be arbitrarily complicated. How should this work with Futhark's [execution
model for compiled code](2019-10-25-beating-c-with-futhark-on-gpu.html), which
is entirely driven by invoking entry points from external code, and what are the
implications for the cost model? It would clearly be very bad to redundantly
re-evaluate constants whenever they are used (since they can be expensive).
Instead, our solution is to evaluate all top-level constants when a compiled
Futhark program is first initialised (technically, [when the context object is
created](https://futhark.readthedocs.io/en/latest/c-api.html#c.futhark_context_new)).
This results in a computational cost that is very real, in that some code takes
time to execute, but is invisible to our benchmarking tools. One of my
colleagues has a habit of writing parameterless entry points that simply embed
their workloads in the code and return some value, which [the benchmarking
tool](https://futhark-book.readthedocs.io/en/latest/practical-matters.html#benchmarking)
will happily run, but of course the cost of invoking such entry points is
essentially zero, as the work is done on startup, not when the entry point is
accessed. Incidentally, this is why I am a believer in making benchmarks look
like real programs, that read input dynamically, as otherwise your results
always depend on how bad the compiler is at doing the computation at compile
time.

Another question is about when it is legal to hoist constant expressions out of
function definitions. As an example, imagine that we have a function that binds
an expression that does not use any function parameters:

```futhark
def f x =
  let invariant = iota 10
  in ...
```


It is clearly valid semantically to move the binding of `invariant` to the top
level ([assuming no uniqueness shenanigans](2022-06-13-uniqueness-types.html)),
but is it also a good idea? By promotion to top-level constants, we change the
number of evaluations of the expression from once every time the function is
called, to exactly once over the lifetime of the program. This is clearly a win
in those cases where the function is called more than once, but the compiler
cannot know when this is the case.

Worse, suppose that the invariant computation produces a large value, such as a
big array. Hoisting the computation changes the lifetime of the value from the
same as the function activation, to being the same as the program lifetime, as
top-level constants are not freed until the program terminates (or in an edge
case, after initialisation for those constants that are never used in entry
points). Since values are operationally quite physical things that actually
occupy computer memory, this seems problematic.

From a cost model perspective, this lifetime question is no issue however, since
Futhark's cost model says nothing about *space*, only *time*. The reason for
this is that it is not clear what a useful and realistic space cost model would
look like, as space usage is often proportional with the amount of exploited
parallelism on a concrete machine. The time cost model uses a model of an
"infinitely parallel" machine, and bridges the gap to real machines using
[Brent's Lemma](https://maths-people.anu.edu.au/~brent/pub/pub022.html). This
clearly does not work when we want to treat the degree of parallelism as a
multiplicative factor.

The nice thing about precision is of course that it also says what you *don't*
promise, so we could simply point to the absence of a space cost model as a
license for the compiler to do whatever it wants. Of course, we have loftier
goals for Futhark than to defeat programmers in semantic arguments, so this is
not quite satisfactory. As it stands, the compiler has a somewhat unprincipled
set of heuristics for when it is OK to hoist things out of functions (and
loops!), but it would be nice to eventually put it on a firmer foundation. I
think it is in almost all cases best *not* to automatically hoist invariant code
out of entry points, as programmers can manually do this when desirable, but I
am unsure whether this would inhibit certain desirable programming techniques.
