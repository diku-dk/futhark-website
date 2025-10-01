---
title: The Biggest Semantic Mess in Futhark
description: The language is largely quite clean, but there is one very nasty corner of it.
---

The [original idea behind Futhark](2021-12-19-past-and-present.html) was that
parallel programming (of certain problems) does not require a complicated
language. Indeed, we believed that there was little need for the complexity to
exceed that of the functional languages commonly taught to first year students
at universities. (The complexity of parallel *algorithms* is another matter.)
Overall, I think Futhark has succeeded at that. The meaning of a Futhark program
is fairly easily determined using normal environment-based semantics; even
tricky things like [uniqueness types](2022-06-13-uniqueness-types.html) are
mainly complicated in an operational sense, and to some extent in the type
system - the *meaning* of a program (once it type checks) is obvious. This
semantic simplicity is also evident in the implementation - while the compiler
has lots of [complicated optimisations](2022-11-03-short-circuiting.html) and
[sophisticated transformations](2019-02-18-futhark-at-ppopp.html), the
[reference interpreter](2025-05-07-implement-your-language-twice.html) is
largely straightforward, and quite similar in structure to how a person studying
programming languages would write their first tree-walking interpreter.

You will note that the above paragraph is full of words like *overall*,
*largely* and *fairly* - this is because there is one language feature that has
proven a particularly fertile ground for edge cases and implementation bugs.
That feature is [size types](2019-08-03-towards-size-types.html). In the
following, I will explain why a seemingly simple type system feature has proven
so surprisingly challenging.

## Size parameters

To recap the basic idea, size types allow Futhark functions to impose
constraints on the sizes of their parameters. A simple example is a definition
of the dot product, which takes two arguments that must be vectors of the same
size:

```Futhark
def dotprod [n] (x: [n]f64) (y: [n]f64) : f64 =
  f64.sum (map2 (*) x y)
```

Here `n` is a size parameter that is implicitly instantiated whenever the
function `dotprod` is applied, based on the concrete arrays it is passed. This
by itself is not so difficult. Sizes can easily be incorporated into a type
checking algorithm by treating them as types of a different kind - the details
do not matter, just take my word that it's *fine*. (Writing blog posts is easier
than writing academic papers.)

The main trouble arises when we introduce the ability to use sizes as term-level
variables, like for example the definition of `length`:

```Futhark
def length [n] 't (x: [n]t) : i64 = n
```

When a size parameter is in scope, it can be used in expressions.
Unsurprisingly, the value of a size parameter is the size of the corresponding
dimension in some array. What is interesting is that we can access the size of
`x` without actually mentioning `x` itself. *Intuitively*, we can imagine that
the concrete value of `n` is determined at run time by actually looking at `x`
(say, by counting its elements), and for now this intuition holds.

But now let us consider what happens for a function that takes the number of
columns of a matrix (the "inner length"):

```Futhark
def cols [n] [m] 't (x: [n][m]t) : i64 = m
```

There are now two size parameters, `n` and `m`, and we retrieve the latter. This
case is a little more challenging in the case where `n` is zero - as we cannot
simply retrieve a row and look at it to determine `m` - because *there are no
rows*. Yet an expression such as `cols (replicate 0 (replicate 3 0))` should
still work (and evaluate to `3`). This means we need to extend our notion of how
the values of size parameters are determined, since it cannot be done by looking
at the syntactic form of an array value and counting the elements (since
`replicate 0 (replicate 3 0)` really is just written `[]`). The solution is to
extend our (conceptual and perhaps even concrete) array representation such that
an array always carries a *shape* with it, in addition to its actual elements.
Then intuitively, to determine the value of some size parameter, we still look
for values (such as `x` above) that have that size somewhere, and extract it
from those values.

But now, perhaps unwittingly, we have picked a difficult fight. The problem is
that we sometimes have to create multidimensional arrays without having any
example of an element! Yet we still somehow have to conjure up the right shape
for the array. As an example, consider the `map` function, of the following
type:

```
val map [n] 'a 'b : (f: a -> b) -> (as: [n]a) -> [n]b
```

The element type of the returned array is given by the return type of the
function (`f`) we are mapping with. But if we are mapping over an empty array,
then `f` may never be applied:

```
> map (\(x:i32) -> [x,x,x]) []
[]
```

How, then, are we supposed to determine that the shape of this empty array is
actually `[0][3]`? When the array is constructed, inside `map`, all that is
known is that the outer size is `n` (which is known to be `0`), and that the
element type is some `b`, but we have no value of type `b` we can use to
determine what the shape may be! We do have a function `a -> b`, but we also
have no `a` - all we have is an array of type `[0]a`, which clearly does not
have any `a`s inside of it...

One solution to this problem is due to Barry Jay and explained in the paper [A
Semantics for
Shape](https://www.sciencedirect.com/science/article/pii/0167642395000151). The
idea is that any "shapely" function can be evaluated normally (with a value,
producing a value) or with a shape, producing a shape. A "shapely" function is
therefore one where the shape of the result depends only on the shape of the
input - which rules out functions such as filtering, where the result shape
depends on the *values* as well. This by itself is no problem to Futhark, as we
only want to allow `map`ing with functions that have a predictable result, in
order to avoid irregular arrays.

Using this approach requires us to have two ways of applying a function: for
value and for shape. This is a slight semantic complication, but perhaps we can
live with it. But we also have to get the *input shape* from somewhere - and in
the case of `map`, all we know is that the input has type `a`. Things could be
made to work if whenever a function is invoked, we also receive the concrete
shapes of values of type `a` (assuming this is possible, but because `a` is used
for array elements, we know it must have a meaningful shape). But if we do
*that*, then why not just take the shape from `b` instead and avoid this entire
business of shapely functions?

And indeed, this is the Futhark evaluation model. At any time, a polymorphic
function can inquire about the *concrete* type that a type parameter has been
instantiated with, and extract a shape if necessary. This can then be used to
annotate any constructed arrays with their full shape. Note that this is a
*model* - the interpreter does it this way, because the interpreter is intended
to closely mirror the model, but the actual *compiler* does not of course do it
literally this way, as type parameters do not exist at run time - it just has to
do it in a way that produces the same result. (In practice, it does
[monomorphisation](https://futhark-lang.org/blog/2021-08-02-value-representation.html).)

## Troubles

We didn't do it this way because it was easy. We did it because we thought it
would be easy. Sadly, it has turned out to not be easy. The basic problem is
that we now have an obligation to always know the full shape of any type at all
times (except for those types that can never be used as array elements, but let
us leave those aside for now). This turns out to require machinery more
intricate than standard environment-based evaluation. The fundamental problem is
pretty obvious: we need to also evaluate types along with expressions, *just in
case* they are eventually used to construct an array, and types occur in various
unexpected places.

For example, consider a [module](2017-01-25-futhark-module-system.html) that
defines some local binding `cnt` and a size-parameterised type that refers also
to `cnt`:

```Futhark
module M = {
  def cnt : i64 = ... -- definition does not matter
  type C [n] = [n][n*cnt]f64
}
```

The usual way definitions of polymorphic types such as `type C [n] = ...` works
is that they are added as type constructors to a type environment, and then
instantiated when used.

Now, `M.C [n]` by itself does not have a shape, since `n` is not yet known. At
some point in the future we may end up with an instantiation `M.C [k]` for some
concrete `k`, and when *that* happens we can then compute the shape of the type,
which will be `[k][k*M.cnt]`. But there is no guarantee that `M.cnt` is actually
in scope - it may be some hidden definition inside the module `M`, and even if
it isn't, it's difficult to go from an expression `n*cnt` and give it a meaning
in a *different scope* that it was originally defined in.

Since Futhark is a pure language, we *could*, as soon as we interpret the type
definition of `C`, substitute the result of evaluating `cnt` into its right-hand
side. But this is also uncomfortable: it's a *syntactic* operation, and while
substitution-based semantics are fairly common in theoretical work, they are
undesirable in implementations, because they are quite inefficient - while the
expression `n*cnt` is small, others may also be large. The interpreter is not
just supposed to be a reference implementation; it is also supposed to
demonstrate that the evaluation of Futhark does not *require* any inefficient or
weird operations (even if the concrete code in the interpreter may be naive in
some cases), such as syntactic substitutions - the semantics must be expressible
via symbol-table-based environments.

Our solution is that a type definition captures not just the right-hand side of
the definition, *but also its environment* - that is, type constructors are
*closures*. When at some point in the future we finally instantiate `M.C` and
have a `k` value for `n`, we extend the captured environment with a binding
`n=>k` and evaluate all the expressions in sizes. This is a very strange
implementation that it took us quite a while to work out - if I had more
experience implementing dependently typed languages, then perhaps I would not
find it so weird, as it really just makes type constructors similar to
functions, which they would be in a fully dependently typed language.

## Takeaway

We have had many bugs related to sizes in the interpreter
([2316](https://github.com/diku-dk/futhark/issues/2316),
[2273](https://github.com/diku-dk/futhark/issues/2273),
[2258](https://github.com/diku-dk/futhark/issues/2258),
[2222](https://github.com/diku-dk/futhark/issues/2222),
[2219](https://github.com/diku-dk/futhark/issues/2219),
[2209](https://github.com/diku-dk/futhark/issues/2209)), including other tricky
cases I did not have the heart to discuss in detail. It is by far the part of
the language that has resulted in the most bugs. While we have formalised part
of the size-based type system and shown it sound, we have *not* treated its
interaction with the module system - and the theoretical formulations are
generally based on syntactic substitutions, rather than environments. I *hope*
that the current implementation, which is heavily environment-based, is
fundamentally the right approach, although I have no real belief that there are
no bugs left.
