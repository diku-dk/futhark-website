---
title: "Why are sizes signed?"
description: "Who knows, maybe there is a cool algebra where negative array sizes make sense."
---

In Futhark, the arrays sizes have type `i64` - the type of
signed 64-bit integers.  This is observable whenever we use a [size
parameter](../examples/size-parameters.html) as a term-level variable:

```Futhark
def length [n] 't (xs: [n]t) : i64 = n
```

Since Futhark arrays cannot have a negative size, one might reasonably
ask why sizes are not of type `u64` - the type of *unsigned* 64-bit
integers - which specifically cannot represent negative numbers.  This
question often comes from functional programmers, who are used to
making invalid states unrepresentable through types.  Perhaps they
have even encountered size-indexed vectors such as [Data.Vect in
Idris](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Vect.html),
where the type system is used to guarantee the absence of run-time
errors (having this in Futhark would have [saved me a blog
post](2023-03-17-on-nontermination.html)).  Were I a Zen master, I
would perhaps [respond to such well-meaning questions with
violence](https://www.bowzwestchester.org/2017/06/jun-7-13-bcr-32.html),
but while hacker koans are certainly a [great inspiration as to the
usefulness of beatings in
teaching](http://www.catb.org/~esr/writings/unix-koans/editor-wars.html),
such approaches do not scale beyond the classroom, so I figured I
should write a blog post justifying this part of Futhark's design.

First, let us consider what we would gain with unsigned sizes.  One is
that `iota` would be a total function.  Its type is currently this:

```Futhark
val iota : (n: i64) -> [n]i64
```

Invisible in the type is an implicit requirement that `n` is
non-negative.  If this is violated, the result is a run-time error.
If the parameter type was instead `u64`, such an invalid argument
would not be possible.  Of course, in practice there are still many
values of `n` for which `iota` will not be able to actually construct
an array with `n` elements in computer memory, so totality is
in practice still a bit of an illusion.

Another advantage is that we will know, inside functions, that
variables corresponding to sizes are non-negative.  Well, *we* know
anyway, but now we would know with *with types*!  In Idris this is
useful because Idris does not actually use "unsigned integers" in the
machine sense of the word, but rather [inductively defined natural
numbers](https://www.idris-lang.org/docs/current/base_doc/docs/Prelude.Nat.html#Prelude.Nat.Nat),
that you can perform recursion over.  The types then act as proof
witnesses that help you rule out impossible cases (such as negative
sizes) that you would otherwise have no way of handling.  In
particular, it is useful because Idris vectors have exactly the same
inductive structure as the natural numbers - i.e. they are linked
lists.  In Futhark, arrays are not recursive (and this is crucial for
parallelism), so this advantage would be lost.

Now let's look at the downsides of using `u64` for sizes.  The most
fundamental problem is that sizes and indexes really ought to have the
same type.  Partially this is so we can say things like "an index `i`
for an array of size `n` is in-bounds if `0 <= i < n`".  This becomes
unclear if `i` and `n` are not the same type (or worse, misleading if
we allow some kind of implicit conversion).

Another reason is that whenever we use explicit indexing instead of
just `map`, it is invariably because we want to perform index
*arithmetic*, and unsigned integers are just not very good at
arithmetic.  For example, the subtraction `x-y` might not be
representable as an unsigned integers, even if `x` and `y` are quite
small integers.  Overflow can of course also happen for signed
numbers, but it tends only to happen for very large numbers (which are
rare), while unsigned underflow can happen for numbers close to zero
(which are common).

Another annoyance is that unsigned numbers have no value that is
guaranteed to never be an invalid index, such as `-1`.  These sentinel
values are useful for constructs such as
[`scatter`](../examples/gather-and-scatter.html), where they represent
results to be discarded.  One can of course argue that it would be
even better to use a [sum type](../examples/sum-types.html) with a
distinguished constructor to avoid silent errors where mistaken
out-of-bounds indexes are ignored.

Finally, slicing is problematic.  In Futhark, as in Python, the way to
reverse an `n`-element array `x` is

```Futhark
x[n-1:-1:-1]
```

or with implicit bounds as

```Futhark
x[::-1]
```

Since Futhark uses inclusive start and exclusive end indexes, [as
Dijkstra wills
it](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html),
we must provide `-1` as the end index when computing a reverse slice -
and of course, the stride itself is also negative.  These values are
not representable with unsigned numbers.

While we eventually want to do more advanced verification of Futhark
programs based on sizes and indexes, this will inevitably require
specialised machinery capable of understanding integer ranges
directly, rather than encoding it in the structure of recursive types.
When such machinery is available, using `u64` for sizes has no real
advantage compared to using `i64` along a guarantee that the actual
size is never negative.  In fact, sticking to only `i64` is probably
helpful, as such analyses tend not to be great at handling type
conversions.
