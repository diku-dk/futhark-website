---
title: Uniqueness Types and In-Place Updates
description: An apologia for one of Futhark's more exotic features.
---

I recently wrote about how Futhark had been [relieved of its last
design defect and was now perfect](2022-04-12-the-final-problem.html).
It turns out that wings made of wax melt quite easily.  While I am
falling, I might as well write about one part of Futhark's design
that, while not *crucially* flawed, is somewhat unpleasant.  This
dubious feature is Futhark's uniqueness type system, supporting the
language feature we sometimes (unfortunately) call *in-place updates*.
Although it serves a purpose, its interaction with other language
features is problematic.

## Basics of uniqueness types

The existence of uniqueness types in Futhark is ultimately rooted in a
desire to accommodate a certain style of programming.  Boiled down to a
single construct, we want to support `with`-expressions:

```Futhark
A with [i] = v
```

Semantically, this expression produces an array with the same elements
as `A`, except that the element at index `i` is replaced by `v`.
Nothing fancy, purely functional, and you could easily write such a
function on lists in any functional language.  But in Futhark, we want
to make a further guarantee: the run-time cost of a `with` expression
must be proportional to the size of the value `v`, *not* the size of
the full array `A`.  In practice, this means that copying all of `A`
is not allowed, and so the only straightforward implementation
technique is to perform a write directly to the storage of `A` - what
we call an *in-place update*.  But since Futhark is a purely
functional language, we don't want the user to *observe* that a
destructive update has been performed.  This feature is about the
[*cost model*](2022-01-27-cost-models-are-contracts.html), not of the
*semantics* of the language.

So how do we support such a feature, which we wish to implement with a
destructive in-place update, without turning Futhark into an
imperative language?  Our approach is to introduce type rules that
check that the "old" value of A is never used again, meaning that the
*effect* of the in-place-update cannot possibly be observed.  This is
precisely what Futharks's uniqueness type system makes possible: after
`A with [i] = v` we mark `A` as *consumed* and mark any subsequent use
as a compile-time error.

This has some overlap with [linear and affine type
systems](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems),
where values must be used *exactly once* (linear) or *at most once*
(affine).  Most operations on linear values return a *new* linear
value, that you can use for further operations.  Linear type systems
are particularly useful for resource management - by requiring that a
value must be used *exactly once*, you make it impossible to forget to
free a resource (such as memory).  Only the deallocation function will
accept a linear argument without returning a new linear value.

Linear arrays would permit efficient `with` expressions, but the type
system would be cumbersome.  For example, while linear types have only
a single notion of *use*, in Futhark we wish to distinguish a
non-destructive *observation* from a destructive *consumption*.  An
array can be observed multiple times, but only consumed once.  This is
needed to allow expressions such as the following:

```Futhark
A with [i] = A[i] + 1
```

All observations of an array must happen *before* a consumption (if
any).  The notions of "before" and "after" are defined syntactically,
by specifying an evaluation order.  It could also be done by defining
rules based on actual data dependencies, which is what the compiler
actually does internally, but we judged that at the surface language
level, it's more important to have simple than flexible rules.  This
is not an issue that is particularly important in practice.  It does
mean that the following code fragment will not type-check:

```Futhark
let B = A with [i] = v
let x = A[i]
```

Here `A` is used after it has been consumed.  It works if we flip the
two lines:

```Futhark
let x = A[i]
let B = A with [i] = v
```

## Alias analysis

What makes this entire business particularly hairy is that consumption
of `A` also applies to *aliases* of `A`.  Two values are aliased if
they *might* share storage at run-time.  The rules for aliasing are
syntax-driven and mostly based on intuition (because we have neglected
to document them properly):

* `A` aliases `A`, meaning that after `let B = A`, `B` aliases `A`.

* `A[i]` aliases `A` if the result is *not* a primitive type.

* `if x then y else z` aliases the union of the aliases of `y` and `z`.

* An array literal `[x,y,z]` has no aliases.  Operationally speaking,
  it is freshly constructed.

And so on.  There is a rule for each syntactic construct, and the type
checker has to carefully track aliases even when names go out of
scope, but it is ultimately more convoluted than truly complicated.
And of course, it is always possible to `copy` a value to obtain an
alias-free version, with no *semantic* impact.  I want to emphasise
this part: aliases *never* affect the semantics of the program, *only*
whether it will type check.  An alias-related type error can *always*
be fixed by copying.

### Functions

The real problem occurs for function applications, as syntax-driven
alias analysis is *intra*-procedural.  This is not just for
implementation simplicity, but also to ensure compositionality.  The
types of functions should make it obvious whether they can be
composed, and not be up to some complicated black-box alias analysis.
This means that function types need to contain the following
information:

1. To which extent a function consumes its argument(s), e.g. a
   function that uses `with`-expressions.

2. How the result of the function aliases its arguments, e.g. consider
   a function like `transpose`, which runs in constant time by merely
   returning a "view" of the original array, without copying it.

It is for *this* purpose, describing the behaviour of functions, that
we need uniqueness types, although I think we have diverged so much
from their origin in [Clean](https://wiki.clean.cs.ru.nl/Clean) that
the name is no longer appropriate.

As a starting point, if a function `f` has type `a -> b`, then it does
*not* consume its argument (of type `a`), and its result (of type `b`)
*may* alias the argument.

If `f` has type `*a -> b`, then `f` consumes its argument, and the
result may have aliases.  This last part doesn't matter for a function
that takes a single argument, but for a function of type `a -> *a ->
b`, the result may alias the first `a`.  Such a function that consumes
its argument behaves a lot like a `with` expression.  After an
application `f A`, we may no longer use `A`.

If a function `f` has type `a -> *b`, then it does not consume its
argument, and the result has no aliases.

These rules are relatively simple, but we need an additional
constraint: the result of a function may not alias a global variable.
This is because the type rules only allow two cases: either the result
of a function call to alias the function arguments, or it doesn't
alias anything.  We have no way to express that it may alias some
global variable.  As with all other alias-related errors, this can be
fixed by using `copy` to break the aliasing.

### Uniqueness?

So why are we using the term "uniqueness types" at all?  In the
original design, a type of form `*a` was called *unique*.  It was
supposed to denote that when you had a variable of this type, you held
the *only* reference to that type.  When this is the case, you can
clearly change the value without affecting anyone else.  But we almost
immediately diverged from this simple concept, as we wanted to permit
a style of programming where an array can be "observed" (e.g. indexed)
many times before it is "consumed".  So gradually we have moved to a
design that is more focused on the notions of *consumption* and
*aliases*, as this is what really matters.

## Complicating the types

In principle, we could complicate the type system further.  Imagine a
function that accepts two arguments and returns a pair, each component
of which will alias a corresponding argument.  This is currently not
expressible.  Either a function result aliases *nothing* or it aliases
*all* arguments.  It is not difficult to conceive of a more elaborate
type system where the potential aliases of return values are more
explicit:

```Futhark
(x: a) -> (y: b) -> (a@{x}, b@{y})
```

In practice, this has not yet proven necessary or even useful.  Most
uses of uniqueness in real Futhark programs tends to be quite
straightforward and localised.

A more significant problem is that uniqueness types do not interact
well with higher-order functions.  Consider an application function:

```Futhark
apply : (a -> b) -> a -> b
```

By its type, `apply` accepts a function that does not consume its
argument.  We can define a "consuming apply" of the following type:

```Futhark
capply : (*a -> b) -> *a -> b
```

The programmer then has to pick the right one for each situation.
Although perhaps clumsy, it has not proven a major problem in
practice.  One reason, as usual, is that you can always obtain a
"unique" (alias-free) array through `copy`.  Since this entire
business is solely about the *cost model*, not semantics, copying is
always viable, as long as you don't do it inside loops in ways that
would affect the asymptotic cost of the program.

A slightly more subtle constraint is that a function is not allowed to
consume any free variables.  Example:

```Futhark
let B = copy A                   -- Fresh, no aliases
let f (i: i64) = B with [i] = 0  -- No!
```

At the point of definition, we have no idea how often `f` will be
applied, and since an array must be consumed *at most once*, we forbid
the definition of the function `f`.  As an example of how it could go
wrong, passing `f` to `map` would obviously result in `f` being
applied multiple times.  On the other hand, passing `f` to `apply`
would result in only a single application.

This could perhaps be solved by using an effect system to indicate
which variable would be consumed.  Then `f` would have type

```Futhark
f : i64 ->{B} []i64
```

indicating that it consumes `B`.  The `map` function would then have
type

```Futhark
map : (a ->{} b) ->{} []a ->{} []b
```

indicating that the function passed in must have no effects.  On the
other hand, `apply` would be

```Futhark
apply : (a ->{E} b) ->{} a ->{E} b
```

where `E` is an *effect variable*, indicating that `apply` has the
same effect as the function being applied.  For brevity, we'd probably
write the effect-free function arrow `->{}` as just `->`.  This may
look like an esoteric case, but in Futhark we want to support a
programming style based on pipelines:

```Futhark
x |> f |> g |> h
```

The pipeline operator `|>` is exactly `apply`, although with its
arguments flipped.  Thus, in order to allow such pipelines where some
of the functions consume their arguments, we would need some sort of
*consumption polymorphism*.  Alternatively, we need dedicated
consuming operators.

## Design goals

Apart from enabling a certain implementation technique while remaining
purely functional, the design of Futhark's uniqueness types was also
shaped by other goals.  The main one is simplicity, and particularly
that programmers can ignore this entire business whenever it is not
needed.  We are particularly concerned with not making function types
too complicated in the common case.  The current situation is mostly
tolerable this regard, with the only blemish being that most functions
have a `*` in their return type to indicate that the result has no
aliases.  It turns out that most functions produce "fresh" arrays, and
if this was not part of the types of standard functions, using `with`
would be very annoying as most things would alias each other.
Alternatively, we could change the design to require an annotation for
those functions that produce aliased results, which would instead
complicate the types of `transpose`, `take`, `flatten`, `id`, etc.

One could argue that for a function

```
val map 'a 'b : (a -> b) -> []a -> *[]b
```

we could infer that the result cannot *possibly* alias the arguments,
as the types differ.  There is no way to construct an array `[]b` that
aliases an array `[]a` unless `a = b`, which `map` cannot assume.
This would cut down on noise for certain polymorphic functions, but it
would remain for many other functions.

Generally, the language complexity budget for Futhark is that it
should be at about the level of Standard ML or Haskell 98.  There are
many complex parallel languages.  Our research and design hypothesis
is that a usable and effective high-level data parallel language does
not have to be particularly complicated.  We wish to preserve a
semantics that is basically the lambda calculus with arrays, and in
which programs are expressed using straightforward transformations of
values.  The `with` construct is a good example of this idea, and I
have long regretted that we used the term "in-place updates" for this
language feature, as it needlessly conflates the implementation with
the semantics.

An open question is whether this feature is really *needed*.  After
all, Futhark is a parallel and functional language.  Why do we need a
feature that is mostly useful when writing imperative-ish code?  The
answer is that many parallel programs contain a sequential *core*
inside all of those parallel loops, and if that core is not efficient,
then neither is the program as a whole.  The `with` expression is also
not the only ultimate source of consumption - `scatter` is a kind of
parallel `with`, and as far as we know it is needed to express certain
highly irregular algorithms, such as graph operations, with the right
asymptotic efficiency.  I really wish we could get rid of in-place
updates, uniqueness types, consumption, aliasing, or whatever you want
to call it, but so far it appears necessary to support the kinds of
programs that we want to write.
