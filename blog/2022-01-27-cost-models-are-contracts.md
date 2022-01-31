---
title: Cost models are contracts
author: Troels Henriksen
description: Make sure to read the fine print.
---

The most precise way of determining the performance of a program is
running it and measuring how long it takes.  Unfortunately, this tells
us nothing about how the program will behave on different workloads or
on another machine.  Also, profiling your code all the time is very
tedious.

Instead, programmers use a *cost model* to reason about the
performance of their code in a more general manner.  We usually (but
not always) disregard small constants, and use [big O
notation](https://en.wikipedia.org/wiki/Big_O_notation) to describe
the *asymptotic* behaviour of a program's runtime relative to the
workload.  Most cost models tend to be informal and even
unstated. Every day we reason in terms of assumptions such as "all
primitive operations take constant time" and "loops execute their
iterations one after another".  These assumptions may seem obvious or
unavoidable, and most programmers are probably not even aware that
they are employing a cost model at all.  Formalising precisely what it
means for performance that `a; b` runs first `a` and then `b` may seem
like pointless academic puffery.  However, in more complicated
operational settings, when we add concepts such as parallelism,
concurrency, or nondeterminism, it stops being intuitive just how fast
some program will be, and writing down a precise cost model can be
quite useful academic puffery.  This also helps programmers and
algorithm designers reason about performance without focusing on
specific machines or implementations.  The cost model becomes a
*contract*: by applying its rules, we get a (usually asymptotic)
*bound* on the "cost" (usually runtime) of a program.  A language
implementation promises that when the program is run in practice, it
must be behave at least as well as promised by the cost model.  Now,
I'm an idealist hacker and the word "contract" does sound unpalatably
business-like to me.  But ultimately a contract is really a *promise*,
and I don't mind making promises to my friends.

We intend to specify Futhark in terms of a fairly simple
*language-based cost model*, although we're not all the way there yet.
In this post I will show some examples to illustrate the idea,
demonstrate how the rules can be used to communicate crucial
operational properties and restrictions, and finally show how simple
cost models (as the one we'll see here) are not able to express things
that are *almost* always true, but cannot be guaranteed.  Cost models
are contracts, which means that we should not break them, but we
should make sure to read the fine print.  We'll also look at some
cases where I think a full formalisation of the cost model actually
ends up inhibiting readability of the details that matter to a
programmer.  I'm still not quite sure myself how to use the cost model
in documentation.

# A language-based cost model for Futhark

First, let us discuss the idea of a *compositional language-based cost
model* (I promise this is the longest italicised term in this post).
The idea is to define a cost function *W(e)* that for some expression
*e* tells us the "cost" of evaluating *e*.  For now this "cost" will
be the total amount of work needed, but it could in principle be any
quantity (e.g. peak memory usage), and we'll see a more exotic cost
function later.

As a starting point, we define that evaluating a constant requires
constant work:

```
W(k) = 1
```

Evaluating an addition *x+y* requires us to evaluate the
left-hand-side, the right-hand side, and finally perform the addition
itself:

```
W(x+y) = W(x) + W(y) + 1
```

Defining the cost of an expression in terms of the cost of its
subexpressions is what makes our model *compositional*: the cost of an
expression does not depend on its context.  This allows us to analyse
the cost of a program in small pieces whose results are then combined,
which is much simpler than having to consider the program as a whole.
The cost model is *language-based* because the cost function is
defined directly on expressions of the user-visible language, rather
than by first transforming the language to some kind of machine
representation.  Cost models that are neither compositional nor
language-based exist, and can be useful, but they are more complicated
to work with.  For example, a cost model for a language such as
Haskell, with lazy evaluation, probably will not be compositional and
language-oriented, since the cost of a term is constant until its
value is needed, which depends on how it is used.  You'd probably need
some kind of machine model to track that.

Even though we have defined just two trivial rules so far, we have
already introduced an interesting detail: the cost of an addition does
not depend on the value being added.  This implies that numbers must
be bounded in size - an implementation that transparently used bignums
would not be faithful to the contract.  And indeed, Futhark's built-in
number types are all of fixed size.

Now let us consider an expression where the cost model *does* depend
on a value at runtime.  The expression `iota e` creates an array of
`e` elements.  If we denote the value resulting from evaluating an
expression `e` as `eval(e)`, we can describe the cost of `iota e` as:

```
W(iota e) = W(e) + eval(e)
```

This means that creating the array of `n` elements takes *O(n)* time.
This allows an obvious implementation strategy: allocate the array and
write each of the values.  But what if we instead defined the cost
like this?

```
W(iota e) = W(e) + 1
```

Now no matter the size of the array we produce, the cost is the same.
This also suggests an implementation strategy: instead of actually
constructing the array in memory, represent it symbolically (or
lazily) as just its size.  This works because the element at position
`i` of an `iota` array is exactly `i`.  Generally, when we define a
cost model for a language, we are free to promise whatever we want,
but each promise makes the language more difficult to implement.
Nothing prevents us from defining a language where everything is
specified to take constant time, but we might find it challenging to
write a compiler that upholds that contract.  This is why Futhark's
`iota` specifies work linear in the size of the resulting array.  In
*most cases*, the compiler will do better than this, and not actually
manifest the array in memory, but there are edge cases where it is not
practical to guarantee constant cost.  I'll return to this later, but
first I want to talk about parallelism.

# A parallel cost model

Work is not the only measure of cost we can define for a language.
Space usage is also an obvious candidate.  But for parallel languages
such as Futhark, the most interesting measure is the
[*span*](https://sigkill.dk/writings/par/cost.html).  Intuitively, the
span *S(e)* of an expression *e* is the length of the longest path in
the data dependency graph that occurs during execution.  Or put
another way, the length of the longest chain of sequential
dependencies.  On an infinitely parallel computer, we can execute a
program with span *s* in *s* steps.  While we don't have infinitely
parallel computers available to us (and if we did, they'd all be stuck
in cryptomining farms anyway), [Brent's
Lemma](https://maths-people.anu.edu.au/~brent/pub/pub022.html) tells
us that we can simulate an infinitely parallel computer on a finitely
parallel computer, with overhead proportional to the amount of
parallelism that we are "missing" relative to what the program could
potentially exploit.  This means that we can use the span of a program
as a theoretical model for quantifying "how parallel" a program or
algorithm is in principle, and expect that this is directly connected
to how it will run on a real parallel computer.

Let us take a look at what a parallel cost model looks like for
Futhark.  First, the span of constants:

```
S(k) = 1
```

Unsurprisingly, constant.  Addition is more interesting, as we have a
choice to make.  One choice is to define the span like this:

```
S(x+y) = S(x) + S(y) + 1
```

This suggests an implementation that first evaluates `x` to
completion, then `y` to completion, then performs the addition - just
as in a sequential language.  Essentially we are told the very useful
piece of information that *addition operator application is not a
source of parallelism*.  Now consider another equally valid way of
defining the span:

```
S(x+y) = max(S(x), S(y)) + 1
```

This tells us that the span is the maximum of the span of `x` and `y`.
This requires an implementation that evaluates `x` and `y` in
parallel, waits for both of them to finish, and then performs the
addition.  Such a strategy is perfectly valid in a pure language such
as Futhark, where expressions cannot have side effects, and any
evaluation order is valid.

So which of these rules for `x+y` do we actually define for the
Futhark cost model?  Even though Futhark is a parallel language, and
so it seems tempting to *maximise parallelism* even in the cost model,
we actually pick the former rule, the one that does *not* promise
parallel evaluation of the operands.  This is because *cost models are
contracts*.  They are not for pointing out what *can* be done, they
are for pointing out what *must* be done: what the programmer can
absolutely rely on, and use to describe the asymptotic complexity of
their programs.  In practice, ensuring *efficient* parallel execution
of everything that could *in principle* be executed in parallel is is
very difficult.  Efficiently scheduling huge quantities of tiny
unstructured and heterogeneous nuggets of parallelism would be a major
research *and* engineering challenge.  So despite parallelism
definitely being *the point* of Futhark, there are actually very few
language constructs that promise parallel execution, with `map` being
the most important one.  A cost model is a clear and precise way of
communicating such details to the programmer.

The cost functions above are quite simple.  If we also want to handle
variable bindings, we need either symbol tables or substitution
semantics.  For example, we might define the work of a `let`-binding
like this:

```
W(let x = e1 in e2) =
  W(e1) + W(e2[x↦eval(e1)])
```

This says that the cost is the cost of `e1`, plus the cost of `e2`
after we first replace any instances of `x` with the result of
evaluating `e1` (in principle we'd also need to define the evaluation
semantics for `eval(e1)` to be meaningful).

The rules get more complex, but not more interesting, as we introduce
other binding constructs such as pattern matching.  If we want loops,
we need to incorporate fixed points of some kind into our cost
functions.  This can get quite technically hairy, and while necessary
if we want to fully formalise or mechanise the cost model, it is not
really useful when using the cost model as part of language or library
documentation.  You can have a perfectly accurate intuition for
something, only to have your confidence shaken by seeing someone
typing it up precisely with a bunch of Greek letters.  I'm not quite
sure where to draw the line in documentation meant for humans.  Maybe
documenting the cost model in terms of a simplified subset, and then
letting the cost of the full language be the "intuitive" extension?
This can perhaps work, but I'm not yet sure how to extend it to
higher-order functions such as `map`.

## The cost of `map`

So speaking of `map`, let us consider how to specify its cost,
starting with the work.  Suppose the expression is `map f x`.
Intuitively, we must evaluate `f` (which can be an expression that
*produces* a function value) and `x`, and then sum the cost of
applying the function to every element of `x`.  Then we define the
work of `map f x`:

```
W(map f x) = W(f) + W(x) + sum({W(f x[i]) for x[i] in x})
```

This is not too bad, since we can precisely specify exactly which
arguments `f` will be applied to.  Note that we have not actually
defined a rule for the cost of the function application `f x[i]` in
this post.  It looks much like the one for `let`-bindings, and is not
otherwise interesting.

Now let's consider the span:

```
S(map f x) = S(f) + S(x) + max({S(f x[i]) for x[i] in x})
```

This suggests an implementation strategy: Launch a thread for every
element of `x` and wait for each thread to finish, which means the
total time will be the time of the slowest application.  This is
really useful information, since it also tells the programmer that
load-imbalanced `map`s are a bad idea: they will run at the speed of
the slowest element.  In practice, oversubscription and automatic load
balancing may reduce the impact, but this is not a *promise* made by the
cost model.  This guides us in the way we write programs.

Interestingly (or embarrassingly), the GPU backends of the Futhark
compiler actually break this contract.  Since [our flattening
algorithm](https://futhark-lang.org/blog/2019-02-18-futhark-at-ppopp.html)
currently only supports *regular* nested parallelism, any inner
instances of `map` will be sequentialised if their size is variant to
any of the outer parallel levels.  If we wanted to express this in the
cost model, it would no longer be compositional, since the span of an
expression would depend on where it appears.  Improving the compiler
to support full flattening when needed is [part of the
plan](https://futhark-lang.org/blog/2021-12-19-past-and-present.html#full-flattening),
and the multicore backends faithfully implement the cost model right
now.

Another interesting case is `reduce op ne x`, which reduces an array
`x` with the binary operator `op` that has neutral element `ne`.  What
is the span of a reduction?  If you [took our
course](https://github.com/diku-dk/dpp-e2021-pub) you will know that
for an array of length *n*, the span ought to be *log(n)*, but what if
the reduction operator itself does not have constant cost?  For `map`
we could easily enumerate all the arguments that we passed to the
function `f`, but the reduction operator `op` will be applied not just
to elements of the array `x`, but also to results of previous
applications of `op`.  And the *precise* values depend on the specific
order of evaluation used by whatever reduction algorithm the language
implementation happens to use, which is something we *definitely*
don't want to specify in the cost model ([see here for some different
ways of implementing reduction in
Futhark](https://futhark-lang.org/blog/2019-04-10-what-is-the-minimal-basis-for-futhark.html#parallel-reduction)).
Almost no reduction operator will actually be ill behaved (most of
them do constant work on scalars), but *cost models are contracts*, so
we should be sure we do not promise too much.  Currently, this is the
best I can come up with:

```
S(reduce op ne x) = S(op) + S(ne) + S(x) + log(length(eval(x))) * W(op a b)
```

where `a` and `b` are those hypotehtical values that would lead to the
*slowest possible* execution of `op a b`.  In parts of the
documentation you might also see that we simply say *W(op)* and
understand this to mean the worst-case cost of applying *op* to
*whatever it happens to encounter during execution*.  We also tend to
not mention the cost of evaluating the `reduce` arguments `ne` and
`x`, as this is *obviously* needed and always the same, and so not
particularly interesting.

The reason I am so concerned with `reduce` (and don't want to make it
too complicated) is that there is actually something very interesting
in the above rule that risks disappearing among the bookkeeping: the
span of `reduce op ne x` depends on the *work* of `op`!  This tells us
precisely that Futhark's `reduce` *does not exploit any parallelism in
the reduction operator!*  That can be quite important to a programmer.

This decision could have been made differently, but was picked to
nudge the programmer in the direction of writing code that is easy to
execute efficiently on a parallel machine.  While is is possible to
implement `reduce` such that it can exploit inner parallelism in the
reduction operator (and you can do so yourself), it is tricky to do so
efficiently, and in most cases not worth it.

# The even finer print

A cost model is a contract that makes promises about the worst case.
Unfortunately, there are some language constructs that are *almost
always* asymptotically faster than in the worst case.  For example,
the `reverse` function is actually implemented by returning a *view*
of the array, not by allocating space for a new array and copying the
array elements in reverse order.  This means `reverse` runs in
constant time no matter the size of the array.

In Futhark, views are not part of the type system.  To the user,
`reverse` returns an array.  Inside the compiler, such views are
implemented by tweaking how arrays are mapped to memory blocks.  If
`ys=reverse xs`, then both `xs` and `ys` are associated with the same
piece of memory, but differ in the *index function* that maps array
indexes to memory offsets.  Such index functions are not stored
dynamically as part of the array, but are a piece of compile-time
information that is used to generate the actual index calculation code
during code generation.  (This reminds me that we should really write
a post on Futhark's internal array/memory representation some day -
it's quite nifty, especially after [Philip
Munksgaard](https://munksgaard.me/)s improvements).

Unfortunately, there are some cases where those array views cannot be
maintained and we are forced to manifest the array in memory.  One
case is when the array is returned from an entry point.  Another is
when it is returned from an `if` branch where the array returned by
the other branch uses a substantially different index function
(typically this requires heavy abuse of all of
`flatten`/`unflatten`/`transpose`/`rotate`).  Although these cases may
be uncommon, we should still be careful to make only promises that we
keep.  I see two options:

1. Specify the cost of `reverse`, `transpose` and similar index
   transformations as having cost proportional to the size of the
   array, suggesting that they actually perform copies.

2. Specify that the cost of `if` and every similar construct has cost
   proportional to the size of the arrays they return, as this might
   force manifestation of views.

Option *2* is attractive because it places the cost where it
operationally actually happens.  However, I think option *1* is
actually more elegant, because the cost is more directly associated
with the construct that produces a value.  Further, in almost all
cases where we use an array view, we soon after perform some operation
(such as `map`ing the array) that has at least the same cost as a
copy, so at a program level, the total asymptotic cost will be the
same.  Also, option *1* allows for naive implementations (such as our
very own interpreter) that don't bother with any of this "view"
business and just put together the new array as instructed.

This is a distinction that it is unfortunately difficult to be precise
about: when making promises we must talk about the worst case, and I
don't know of a simple way to precisely quantify that the common case
is *much* faster.  We could certainly formalise it fully, but I don't
think the resulting model would look very simple.  A promise is no
good if the recipient does not understand what is being promised.
