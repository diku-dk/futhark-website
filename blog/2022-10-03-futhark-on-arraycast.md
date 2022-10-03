---
title: Futhark on The Array Cast
description: Mingling with the real array languages.
author: Troels Henriksen
---

The [latest episode of The Array
Cast](https://www.arraycast.com/episodes/episode37-futhark), the
worlds premier podcast on APL and related matters, had me on as a
guest to talk about Futhark and array programming.  This was a
continuation of [the preceding
episode](https://www.arraycast.com/episodes/episode36-what-makes-an-array-language),
which focused on what makes a language an array language in the first
place.  We tend to call Futhark an array language, despite
significantly deviating from most such languages, and I suppose the
Array Cast panel was curious about how and why.  I had a delightful
time, and I recommend listening not just to the episodes linked above,
but also the podcast in general.  Most episodes are quite accessible
even to people with no APL experience - although it doesn't take more
than an afternoon to pick up enough APL to get the gist of things, so
why not do that?.  Still, unaccustomed as I am to public speaking, I
feel there are a few things I did not make sufficiently clear, so
here's a followup post with elaborations.

## Rank polymorphism

First: *rank* is the number of dimensions of a multidimensional array.
For example, a vector has rank 1, and a matrix has rank 2.  Perhaps
the defining trait of array programming is *rank polymorphism*.  This
is the idea of having functions or operators that apply can be applied
to arrays of any rank.  The simplest example is a `+` operator that
can be applied to vectors, which then adds elements piecewise:

```
[1,2,3] + [4,5,6] == [5,7,9]
```

Squinting a bit, and using terminology from functional programming,
rank polymorphism can be seen as every function implicitly `map`ing
over its arguments.  In some implementations of rank polymorphism,
such as the APL successor [J](https://www.jsoftware.com/), this is
achieved by a consistently applied set of rules such as [leading axis
theory](https://aplwiki.com/wiki/Leading_axis_theory).  In others,
such as original APL or the Python library NumPy, it is more ad-hoc,
and can possibly be customised by the programmer.  In general,
programmer-written functions can inspect the shape of their arguments
and make decisions based on what they find.

## Problems

Futhark does not have rank polymorphism in any form.  You have to
write out the `map`s yourself.  There are two reasons for this:

1. Futhark was not originally conceptualised as a language in the APL
   tradition, but rather as a restricted ML dialect.  Futhark's arrays
   are an evolution of ML *lists*, not the multidimensional arrays of
   APL.

2. Rank polymorphism is tricky to handle in a type system.

Point 1 can be rephrased merely as "we didn't think of it" and is not
worth discussing further, but point 2 is more interesting.  There are
indeed statically typed array languages, but they are rare.  One
interesting case is [Single-Assignment C](https://sac-home.org) (SAC),
a statically typed array language intended for high-performance
parallel execution.  It looks superficially similar to C, but it is a
pure functional language with strong APL influences, and it supports a
rich variant of statically typed rank polymorphism.  SAC did highly
efficient functional programming long before it was popular, yet is
strangely little known.  I wonder if this is because the initial work
on SAC came at the tail end of the parallelism enthusiasm of the 80s,
and right at the beginning of the incredible gains in *sequential*
performance that occurred in the 90s.  Beyond its performance, SAC
also made innovations in rank polymorphism, and in particular supports
the notion of rank *specialisation*, where a function can contain
specialised definitions for certain shapes of input.  This can be used
to handle special cases more efficiently, but I also recently read [a
paper](https://dl.acm.org/doi/abs/10.1145/3520306.3534500) that used
rank specialisation of a recursive function to explore the design
space of algorithms for [prefix
sums](https://en.wikipedia.org/wiki/Prefix_sum).  Rank polymorphism is
most often used as a fairly straightforward shorthand, so I find it
exciting when it is used as a more subtle control flow mechanism.

SAC has a static type system and supports rank polymorphism, but not
much else.  In particular, SAC does not support [parametric
polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism).
It is my impression that rank polymorphism is challenging to support
alongside many other desirable type system features.  The most
advanced work in this area is
[Remora](https://www.ccs.neu.edu/home/shivers/papers/rank-polymorphism.pdf),
which is essentially a statically typed APL (minus the notation -
Remora uses s-expressions).  Remora is impressive work that deserves
more careful study from both myself and others, but it is also very
*complicated*.  Perhaps too complicated to be useful by the usual
target audience of array languages, which is traditionally thought to
be "domain experts" and "problem solvers", meaning those well-adjusted
people who are primarily concerned with something *beyond* the
programming language itself.

As an example, consider a language that supports both rank
polymorphism and parametric polymorphism.  Now consider a function of
the following type:

```Futhark
val tally 'a : [*]a -> i64
```

I'm using pseudo-Futhark syntax, because that's the blog you're
reading.  The `[*]` notation is stolen from SAC and indicates that the
array argument to `tally` can be of any rank.  The `tally` function
returns the total number of elements in the argument.

Sounds simple, right?  But what happens if we apply `tally` to a
matrix, e.g:

```Futhark
tally [[1,2,3],[4,5,6]]
```

Whenever we check a polymorphic function application, we have to
instantiate the type parameters (here `a`) with a concrete type.  But
do we with `a=[3]i64` meaning the array argument has shape `[2]` and
the function returns 2, or with `a=i64` such that the argument has
shape `[2][3]` and the function returns 6?  As you can see, this is
semantically significant!

We can come up with a rule: we always instantiate such that shapes
cover as many dimensions as possible.  But this makes type inference
more difficult.  If we have an application `tally A` and we're not
*quite* sure what `A` is yet, what do we do?  This is particularly
difficult for more complex functions where the rank of the input also
affects the rank of the output.  As I understand it, most of Remora's
complexity comes from the desire to answer such questions.

## Rank polymorphism in future Futhark

Current Futhark programming style involves typing `map` a lot.  This
can get pretty verbose, but we have been reluctant to add complexity
to the type system.  However, we think we have an idea for how to
support a very simple form of rank polymorphism.  The idea is that
instead of making *functions* rank-polymorphic, we do it for *function
application*.  Intuitively, our idea is that when the type checker
sees an application

```
f A
```

that would be a type error just from the types of `f` and `A`, it will
see if the application can be made well-typed simply by adding enough
`map`s on top of `f`:

```
(map (map f)) A
```

That's basically it.  It also generalises to binary operators, such as

```
A + B
```

which is equivalent to

```
(+) A B
```

and can thus be rewritten to

```
map2 (+) A B
```

There will certainly be cases where it doesn't work, such as if the
type of the function or its arguments is not sufficiently well known
at the point of application, but in *most cases* this "automatic `map`
insertion" should do the right thing.  And when it *doesn't* and the
type checker complains, the programmer can just insert the `map`s
manually.  This facility is purely a shorthand, and explicit `map`ping
is still available.

Is this as powerful as actual rank-polymorphic functions?  No.  For
example, the `tally` function used above cannot be implemented.  But
automatic `map` insertion (marketing name: **AUTOMAP**) fits well
within Futhark's ML-flavoured type system.

## Does Futhark reject all of APL?

If Futhark was designed as an ML, and it rejects the two main
characteristics of APL-style languages (concise notation and rank
polymorphism), does that constitute a total rejection of all things
APL?  Not at all!  Beyond its merits as a language, APL and its
successors remain interesting as a source of *decades* of experience
expressing algorithms and logic as bulk transformations of arrays,
much of which are in principle *parallel*, even if most APL
implementations are actually single-threaded.  Early APL provided a
programming vocabulary for *humans* that encouraged thinking in terms
of bulk operations on arrays.  It's a remarkable coincidence that this
also turned out to be a decent fit for vector *machines* invented
decades later.  Ken Iverson's Turing Award Lecture discussed [Notation
as a Tool of
Thought](https://www.eecg.utoronto.ca/~jzhu/csc326/readings/iverson.pdf),
and I think the style of thinking (and programming!) encouraged by APL
and its relatives is still worth studying and incorporating into our
daily work as programmers.
