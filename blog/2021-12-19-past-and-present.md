---
title: The past and present of Futhark
description: Where we came from, where we are bumbling towards.
---

We recently released Futhark 0.20.1, which introduces the `def`
keyword for top-level definitions.  This means that [all known
language design flaws](2019-12-18-design-flaws-in-futhark.html) have
now been fixed (as I no longer consider higher-order modules a design
flaw), and hence Futhark is now perfect.  So what comes next?  Will
the language never change again?  Will we go work on some other
project instead?  The answer to both questions is "no", but this seems
like a good opportunity to discuss some of the original motivation
that lead to Futhark, and where we will go from here.  I have
previously written a [navel-gazing
narrative](2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html)
about my own involvement, but Futhark's past and probable future goes
beyond just me.  Futhark is after all developed by a team with diverse
interests and motivations.

The genesis of Futhark came about when [Cosmin
Oancea](http://hjemmesider.diku.dk/~zgh600/) was hired by the
[HIPERFIT](https://hiperfit.dk) project.  HIPERFIT was a research
project that investigated the use of high-performance functional
programming to solve financial problems.  As a postdoc, Cosmin had
done a lot of work on automatic and speculative of parallelisation of
Fortran programs at Texas A&M and Cambridge, for example on the
[Polaris
compiler](http://polaris.cs.uiuc.edu/polaris/polaris-old.html).  The
compiler research community is full of ideas that have not been tried
much in practice, because the time it takes to implement an idea can
be very high, due to the friction involved in working within the
framework of an existing compiler and language.  Cosmin conceived of a
tiny functional language, really not much more than a [compiler
IR](https://en.wikipedia.org/wiki/Intermediate_representation), on
which certain experimental compiler optimisations would be reasonably
easy to perform.  That the language was given a syntax at all was
mostly a convenience for writing benchmark programs.

Over the years, that tiny language eventually became Futhark, which
also grew to be much more than just an IR.  Now it's even downright
pleasant to use!  This transformation occurred because language design
turned out to be quite fun.  We truly did not want to design a new
language, and even spent some time initially trying to find existing
languages that we could use as a frontend to the compiler.  But from
time to time we added minor features to the language to ease the job
of writing benchmark programs, and sometimes we realised that there
was even interesting research problems when it comes to designing a
language for efficient execution.  [Martin
Elsman](https://elsman.com/) contributed significantly to adding
features such as polymorphism, modules, and higher-order functions,
with [Anders Kiel Hovgaard](https://ahovgaard.dk/) contributing
substantially to the latter.

While the original idea that *the language isn't the point* is not
strictly true anymore, we really are not trying to do any grand
language design experiments.  There are lots of very nice languages
out there - Haskell is one of my own favourites - and they keep
improving.  Futhark isn't as nice or as flexible as the best of them,
and likely won't ever be.  But Futhark is hopefully *faster*.  Our
research challenge, from a language perspective, is to identify those
language features that can help the compiler (or at least not hinder
it), and are useful for the kinds of nested parallel number-crunching
that Futhark is meant for.  We also want the language to stay simple -
my own yardstick is that Futhark should in aggregate be no more
complicated than [Standard
ML](https://cs.lmu.edu/~ray/notes/introml/).

So while the idea that Futhark is now *perfect* is of course
facetious, it is indeed the case that significant innovation in
language design is beyond the original scope of the project.  The
future of Futhark does involve further language improvements, but
mostly more work - and research! - on the compiler side of things.
For the rest of this post, I'll cover some of the things I hope we'll
be working on in 2022.

# Broadcasting

I've heard it said that "array programming means not having to write
`map`".  Well, in Futhark you do need to write `map`, and while it can
sometimes lead to useful clarity and flexibility, you end up writing
`map` *a lot*.  In particular, when transcribing linear algebra
formulae, the `map`s become quite noisy.  Recently I found myself
writing `map2 (map2 (/)) x y` to element-wise divide two matrices,
which mathematically we would write as just `x / y`.

Most statically typed array languages support this by having a notion
of [rank
polymorphism](https://prl.ccs.neu.edu/blog/2017/05/04/rank-polymorphism/)
in their type system.  [Single-Assigment C](http://sac-home.org/)
(SAC) is an example of this.  In SAC, the division function would be
defined as accepting arrays of *any rank and shape* (even different
ones), with rules for how a value is then "broadcasted" across another
value of higher rank.  This allows us to mix arrays of different rank,
such as when multiplying a vector with a scalar.  In such a system,
scalars are just arrays of rank zero.  The venerable APL was probably
the first language with such rules, and they have since become very
common through libraries such as NumPy.

Rank polymorphism is a big hammer that interacts in complex ways with
parametric polymorphism and higher-order functions.  I would very much
like to do a proper analysis on the pros and cons of the different
approaches one day, but I think Futhark irrevocably went down a
different path on this one.  We don't want rank polymorphic function
types, but we also don't want to write `map` quite as much.  Is there
a middle ground?

Well, that is possibly a research question.  We believe that we can
come up with a system where the compiler automatically inserts `map`
and `replicate` operations "as necessary" to make the program
type-correct.  For example, consider the case of dividing two matrices
element-wise.  We'll use a division function instead of an operator,
just to make the syntax simpler:

```
val div : f32 -> f32 -> f32
```

Suppose we have arrays `xss : [n][m]f32`, `yss : [n][m]f32`.  The
expression `div xss yss` is ill-typed, but it becomes well-typed
simply by wrapping `div` in two `map2`s:

```
map2 (map2 div) xss yss>
```

Intuitively, this wrapping "lifts" a function to operate on values of
higher rank.  Now suppose we have a scalar `y : f32` and we wrote `div
xss y`.  Again, this is ill-typed.  But we can make it well-typed by
inserting `map2`s to lift `div` and some `replicate`s to add extra
dimensions to `y`:

```
map2 (map2 div) xss (replicate n (replicate m y))
```

The type of `div` is still the simple `f32 -> f32 -> f32` that
operates on scalars.  Instead of supporting rank polymorphism in
functions, *we support it for function applications*.  This is
certainly weaker than full rank polymorphism, but it is probably
powerful enough to remove the noise of `map`s that plague many Futhark
programs.  What I like is also that it's ultimately just a shorthand -
you can write out the `map`s and `replicate`s yourself if you are
uncertain about what is going on, and the compiler can quite easily
explain what it has inferred, in terms of the same language that the
programmer is using, and without adding *any* new concepts to the type
system.

Questions left to answer:

* It is sometimes ambiguous where and how to insert the `map` and
  `replicate`s.  For example, `id xss`, `map id xss`, and `map (map
  id) xss` are all well-typed.  Is a rule such as "always insert the
  minimal number of `map`s" enough to disambiguate?

* What happens when higher-order functions become involved?  How do we
  see that `yss |> div xss` should be turned into `yss |> map2 div
  xss` when the actual application is hidden in the definition of `|>`?

* Which other languages have done similar things?

# Automatic differentiation

Automatic differentiation (AD) is a program transformation that is
used to compute the derivatives of an arbitrary function.  We can view
AD as a generalisation of the symbolic differentiation rules taught in
high school calculus, extended to also work for programs with control
flow, state, storage, etc.  Computing the derivative of a function is
a useful building block for [function
optimisation](https://en.wikipedia.org/wiki/Mathematical_optimization),
where we are trying to find an *x* such that *f(x)* is minimised or
maximised.  For example, modern machine learning makes heavy use of
[gradient descent](https://en.wikipedia.org/wiki/Gradient_descent) as
part of
[backpropagation](https://en.wikipedia.org/wiki/Backpropagation).
While AD is not a new idea, the machine learning renaissance has
caused a substantial growth in interest, as better AD techniques let
us train more sophisticated models.

We (the Futhark developers, on Cosmin Oancea's initiative) have spent
a lot of 2021 implementing a quite effective AD transformation in the
Futhark compiler.  I expect to write a *lot* more about it once we
make it available in an actual release, so I will not go into a lot of
detail about how it works and what it's useful for.  What is
interesting is that, so far, AD has not required any language
extensions.  Two higher-order functions serve as the API:

```
val jvp 'a 'b : (f: a -> b) -> (x: a) -> (x': a) -> b

val vjp 'a 'b : (f: a -> b) -> (x: a) -> (y': b) -> a
```

This is the "ideal interface" of [Barak
Pearlmutter](http://www.bcl.hamilton.ie/~barak/) and are surprisingly
flexible.  To the compiler, these are of course deeply magical, and
trigger AD transformation of the provided function.  While a language
such as Haskell is powerful enough to implement [AD as a
library](https://hackage.haskell.org/package/ad), Futhark requires it
to be baked into the compiler.  One question is how Futhark's AD
support will fare compared to languages such as
[Dex](https://github.com/google-research/dex-lang).  Futhark was
co-designed with an optimising compiler, and so optimises well, but AD
was never part of its original design.  In contrast, Dex is designed
by AD experts and specifically with AD in mind.  It will be
interesting to see we will ultimately be hindered by our language
design.  But so far, it actually works surprisingly well, and our
[embryonic AD benchmark suite](https://github.com/diku-dk/futhark-ad)
shows both excellent run-time performance and a rather pleasant
programming experience.

And honestly, if people managed to write [useful AD transformations
for Fortran](http://tapenade.inria.fr:8080/tapenade/index.jsp), surely
we can cobble something together for Futhark.

# Accumulators

Futhark is a pure language, so side effects are tightly controlled:
there are none.  (Except non-termination, if you are a purist.)
Things that would normally be done with side effects are instead
handled through other language constructs.  For example, an "in-place
update", where we want to change the value of one element of an array,
is written as `xs with [i] = v`.  *Semantically*, this gives you a
copy of `xs` with a new value at index `i`.  *Operationally*, this is
implemented by an immediate (and cheap!) write to whatever memory is
actually storing `xs`.  To ensure this imperative write cannot be
observed as an effect, `xs` and its aliases may not be used on any
execution path following the `with`-expression.  The type checker
enforces this via [a combination of alias analysis and uniqueness
types](https://futhark.readthedocs.io/en/latest/language-reference.html#in-place-updates).

After an expression `xs with [i] = v`, we say that `xs` has been
"consumed".  One important safety principle is that a value is only
consumed once.  But consider an expression such as the following:

```
map (\y -> xs with [0] = y) ys
```

Since the function we pass to `map` will be executed once per element
of `ys`, we will end up consuming `xs` multiple times.  Not good.
When defining a function, even an anonymous one, we have no idea how
often it will be invoked.  The solution is a rule, enforced by the
type checker, stating that we cannot consume any variable not bound
within the "current function", since that might otherwise result in
the variable being consumed multiple times.  This rigid rule has not
really been a problem so far, but it may be about to become so.

Because `with`-expressions don't mesh well with higher-order
functions, Futhark provides the `scatter` function for performing
parallel updates.  For example, `scatter xs is ys` returns the array
`xs` modified such that `xs[is[i]] = ys[i]` for every pair
`(is[i],ys[i])`, and treats the old `xs` as consumed.  (Incidentally,
I always felt that `scatter` should be a syntactic extension of
`with`-expressions rather than a function.)

The main restriction of `scatter` is that you can only do one "update"
per input element.  For some irregular algorithms, the amount of
updates might be a dynamic data-dependent quantity.  To address this,
we would like to introduce the notion of an "accumulator":

```
type acc 't
```

A value of type `acc t` represents a "write-only view" of an array
with element type `t`.  Similar to how a `with`-expression can update
an array, we provide a function for updating an accumulator:

```
val upd 't : *acc t -> i64 -> t -> acc t
```

The `*` before the accumulator parameter means that the argument is
consumed and may not be used again.  Instead a new accumulator is
returned.  By requiring such a *linear* use of accumulators (in the
[linear
types](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems)
sense) we ensure that data dependencies are clear.  By making
accumulators write-only, we allow multiple "threads" to operate on
them concurrently without opening the door to nondeterminism.

Accumulators are brought into the world by a higher-order function:

```
val with_acc [n] 't :
  (dest: *[n]t) -> (f: *acc t -> acc t) -> [n]t
```

Intuitively, `with_acc` temporarily turns an array `dest` into an
accumulator, passes it to the provided function which returns a
modified accumulator, and then turns the accumulator back into an
array, which will reflect the updates performed.

This leaves a lot of questions still to answer:

* How do we allow parallel updates?  We cannot consume an outer
  accumulator from within a lambda we pass to `map`, due to the safety
  rule mentioned above.  Do we just `replicate` accumulators and `map`
  them?  Then how do we turn the resulting array of accumulators back
  into a single one?

* What are the semantics for accumulators?  If we require that they
  are used in an exclusively linear manner, then they can be modeled
  as lists of index-value pairs; recording updates that are then
  applied to an array at the end.

* If we allow multiple accumulators to exist concurrently for
  different arrays with the same element type, how do we check that
  the function passed to `with_acc` returns the right one?

Futhark does not have full linear types, but our uniqueness types can
model some of it, if we are careful about which functions we expose.
But we do lose flexibility, as it means only a single accumulator can
usefully exist at once - the others will become "unconsumable" in
nested uses of `with_acc`.  It might be nice to have a limited form of
linearity for higher-order function types, such that we can
differentiate between `with_acc` which applies its function *once*
(and so in principle needs not be restricted with respect to
consumption), and `map` which applies its function *many times*.

Accumulators are already fully implemented in the Futhark core
language, as they are a crucial building block for the AD
transformation.  This was a much easier problem to solve, as the core
language is monomorphic and first-order, except for certain built-in
second-order constructs that have known semantics.  It's really quite
a challenge to figure out how to expose this power to the source
language in a safe and sound manner.

Of course, one might argue that the only reason we have such trouble
is because Futhark is too stubbornly pure.
[Dex](https://github.com/google-research/dex-lang) solves this though
the use of an effect type system, where their main looping construct
(`for`) is polymorphic in the underlying effects, and all this
business with accumulators is expressed as just an accumulation
effect.  I don't think retrofitting a full effect system is a good
idea for Futhark, however, as it's too much of a divergence from the
original design philosophy.  I don't mind that languages aren't good
at *everything* as long as they are really good at *something*.

# Full flattening

One of Futhark's original research challenges was how to efficiently
map nested parallelism to GPUs and similar restricted processors.
This problem had already been solved in principle by
[NESL](http://www.cs.cmu.edu/~scandal/nesl.html) and in practice by
[CuNESL](https://ieeexplore.ieee.org/document/6337595).  These
solutions employ the very general *full flattening* algorithm, which
always works but often produces somewhat inefficient code, in
particular with respect to space usage and locality.  For Futhark, we
focused on a subset of parallelism, namely *nested regular
parallelism*, for which we believed a less general technique could
produce much more efficient code.  This worked, and handles a useful
class of programs, and any instances of irregular nested parallelism
would usually result in either sequentialisation or a compiler error
message.

The later [work of Duc Minh
Tran](https://futhark-lang.org/student-projects/duc-msc-thesis.pdf)
gave us a multicore backend that can handle *any* nested parallelism,
but it always bothered me that there are valid Futhark programs that
we cannot compile for GPU execution.  I plan to take a serious look at
implementing full flattening in the Futhark compiler, as part of the
multi-versioned code generation scheme used by [our incremental
flattening
algorithm](https://futhark-lang.org/blog/2019-02-18-futhark-at-ppopp.html).
First I just want to get it to work, but I also have some ideas for
(hopefully common) special cases that can be handled more
efficiently - in particular for the (common?) case where the outer
levels of parallelism are regular, and only an inner one is irregular.

I long ago decided that the criteria for releasing version 1.0 of the
compiler would be the ability to compile any valid program to GPU
code, and this is the last piece missing.  Hopefully 2022 will see
that happen.
