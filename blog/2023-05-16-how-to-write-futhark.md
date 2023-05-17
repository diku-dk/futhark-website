---
title: How should Futhark be written?
author: Troels Henriksen
description: When we add enough features, suddenly we have choices to make.
---

It occurs from time to time that programmers disagree on how to write
a program.  The risk of this happening increases when objective
metrics, such as safety or performance, cannot be used to make a
decision.  When the difference between two otherwise equivalent
programs is down to subjective values such as concision, elegance, or
style, it can be difficult to decide how to proceed - even for a
community as consensus-seeking and conciliatory as programmers.

For a long time, Futhark was a feature-impoverished language, and
apart from things such as naming, there was not much to disagree on.
We programmed with bulk operators such as `map` because otherwise our
program would not be parallel, and we refrained from explicit indexing
because otherwise our program would not
[fuse](https://en.wikipedia.org/wiki/Loop_fission_and_fusion).

Over the years the language has slowly accrued more features, and now
programmers have access to many more things to disagree about.  While
Futhark is not a strongly opinionated language ([it is meant to be a
polite
guest](2018-06-18-designing-a-programming-language-for-the-desert.html)),
the built-in [prelude](../docs/prelude/) implicitly encourages a
specific style of programming.  While programmers can ignore that
style and go their own way, we should not ignore the impact of
programming style in examples and documentation.  Especially
extensions to the type system, such as [size
types](2020-03-15-futhark-0.15.1-released.html), raise new questions.

Consider the `flatten` and `unflatten` functions.  In primordial
times, these were aspects of a built-in `reshape` construct with
bespoke syntax, but when these were first defined as ordinary
functions, they had these types:

```Futhark
val flatten   't : [][]t -> []t
val unflatten 't : i64 -> i64 -> []t -> [][]t
```

To use `flatten` we would say `flatten xss` and receive back a
one-dimensional array.  This could not fail.  To use `unflatten` we
would say `unflatten n m xs` and receive a two-dimensional array.  This
would fail at run-time if `xs` did not have exactly `n*m` elements at
runtime.

When we added sizes to the type system, these functions were assigned
more informative types:

```Futhark
val flatten   [n][m] 't : [n][m]t -> ?[k].[k]t
val unflatten [k]    't : (n: i64) -> (m: i64) -> [k]t -> [n][m]t
```

For `unflatten`, it is now clear that the shape of the result
corresponds exactly to the `n` and `m` parameters - although with a
run-time error if `k != n*m`.

For `flatten`, the type system was not flexible enough to express that
the result array has a size that is a product of `n` and `m`, so
instead it was given an *existential* size `k` that cannot be known
statically.  This was often cumbersome, so we added a utility
function:

```Futhark
val flatten_to   [n][m] 't : (k: i64) -> [n][m]t -> [k]t
```

With `flatten_to`, the caller can *promise* the size of the result.
We'd often compute `let nm = n*m` sometimes in advance, and then use
it as `flatten_to nm xss`.  If the promised size does not match the
actual size, the result is a runtime error.  As such, `flatten_to` is
strictly speaking not safe.  Indeed, it is *less safe* than plain
`flatten`, as `flatten` cannot fail.  However, many programs would
afterwards need a [size coercion](../examples/size-coercions.html) in
an ad-hoc way to adjust the size of the `flatten`.  In practice,
`flatten_to` encourages a programming style where we pre-compute sizes,
and the type checker verifies that we at least use them consistently.

With the recent and ongoing work on [arbitrary size
expressions](2023-05-12-size-type-challenges.html), we can yet again
fiddle with these functions.  Here's how they are currently defined:

```Futhark
val flatten   [n][m] 't : [n][m]t -> [n*m]t
val unflatten        't : (n: i64) -> (m: i64) -> [n*m]t -> [n][m]t
```

Note that `flatten` now has a completely precise type.  We give it an
array of shape `[n][m]` and we get back an array of shape `[n*m]`.

Similarly, a partial application `unflatten n m` gives a function that
flattens arrays of shape `[n*m]` - and we get a type error if we pass
it an array whose size does not have that exact structure.  This can
lead to slightly verbose programming, sometimes.  For example,
expression `unflatten 3 3 (iota 9)` is ill-typed, as `9` cannot unify
with `n*m`.  Instead we could write it as `unflatten 3 3 (iota (3*3))`.
More generally, we may need a size coercion to massage the
shape of the array before unflattening it: `unflatten n m (xs :>
[n*m]t)`.  There is still a dynamic check, but now it has been made
the responsibility of the caller - it is a precondition.  If we write
our program carefully, we may be able to ensure that the sizes already
match up.

But this got me thinking, isn't there some redundancy in `unflatten`
now?  Why pass the shape both as explicit integer arguments and in the
array itself?  That led me to this type:

```Futhark
val unflatten [n][m] 't : [n*m]t -> [n][m]t
```

Now we can simply say `unflatten (iota (3*3))` to receive an array of
shape `[3][3]`, as the shape of the argument array uniquely determines
the result.  This is a useful example of the kind of [tricky functions
I mentioned as problematic in my last
post](2023-05-12-size-type-challenges.html#unification).  The case
where a coercion is needed also remains reasonably simple: `unflatten
(xs :> [n*m]t)`.

But `unflatten` is not the only function where we could follow this
*shape-driven* approach.  The `split` function is currently defined
with the following type:

```Futhark
val split [n] 't : (i: i64) -> [n]t -> ([i]t, [n-i]t)
```

Note the implied invariant `i<=n`.  An alternative type is as follows:

```Futhark
val split [n][m] 't : [n+m]t -> ([n]t, [m]t)
```

Where before we would say `split i xs`, now we simply say `split xs` -
assuming that `xs` has a shape of the right structure.  If it does
not, we need to use a coercion: `split (xs :> [i+(n-i)])`, assuming
`xs` has type `[n]t`.

For `split` the gains are a little more dubious than for
`flatten`/`unflatten`, but I still like the idea of moving the "proof
obligation" to the caller.  And more generally, I've become quite
interested in seeing how far we can push this idea of writing
functions that are picky about the *structure* of the sizes of their
arguments.  It is similar to [Dex's typed
indexes](2020-12-28-futhark-and-dex.html), although not as powerful.
I've experimentally ported the [Futhark benchmark
suite](https://github.com/diku-dk/futhark-benchmarks) to use these new
definitions of `flatten`, `unflatten`, and `split`, and it was quite
painless and, in my subjective opinion, a minor improvement in clarity.
I'm really curious whether even more interesting things can be done,
though.
