---
title: Value-oriented programming in Futhark
author: Troels Henriksen
description: Futhark isn't really a functional language - it is a value-oriented language, and while the distinction is not particularly precise, it affects how you think about things.
---

When I started at university, I took a course called *Functional Programming*.
In truth, while we used a functional language (Standard ML), we were actually
taught a closely related but not particularly precisely defined paradigm:
*value-oriented programming*. This term was coined by the responsible of the
course, [Fritz Henglein](https://hjemmesider.diku.dk/~henglein/), and [it's one
of those terms that just kept bouncing around in my head over the
years](2026-01-16-are-arrays-functions.html). In this post I will try to write
down my thoughts on what distinguishes value-oriented programming from
functional programming, why Futhark is really a value-oriented programming
language, why that is useful both for debugging and for explaining programs, and
what a dead and Dutch computer scientist might think about it all.

For the purpose of this post, let us understand functional programming as the
somewhat fuzzily defined discipline of programming with things such as:

1. Pure functions and immutable variable bindings.
2. An emphasis on recursion over iteration.
3. First-class and higher-order functions.
4. Parametric polymorphism.
5. Functions as data structures.

Not all languages commonly called "functional" have all these features, and not
all of them are completely strict about it - many ostensibly functional
languages do not actually restrict side effects, and some do not have static
type systems, and that is *fine*. This is not the thing I want to quibble over.
No, it is actually the last point that distinguishes value-oriented from
functional programming: functional programming originally (and to an extent,
still) emphasises the use of functions as *data*.

One of the earliest papers comparing Haskell against other languages, the
evocatively named [Haskell vs. Ada vs. C++ vs. Awk
vs...](https://www.cs.yale.edu/publications/techreports/tr1049.pdf) featured a
Haskell implementation that implemented areas in the plane as function closures
from points to booleans, returning true when the point was contained in the
represented area. Parser combinators, a great invention, also usually represent
individual parsers as closures, which are then composed using yet other
functions. There's even [a nice rhyme about
it](https://people.willamette.edu/~fruehr/haskell/seuss.html):

> A Parser for Things <br>
> is a function from Strings <br>
> to Lists of Pairs <br>
> of Things and Strings <br>

These representations can be delightfully simple, but they also have a big
downside: functions are *black boxes*. All you can do is apply them - their
internal structure is invisible. Sometimes this is just what we want, but it
also comes with limitations. For example, when taking the union of two areas
represented as functions, we cannot efficiently check if one area is entirely
contained in the other, and not construct a new closure in that case. But worse,
we also cannot (usually) *print* the function to look at and understand its
structure. I am a *big* fan of having a notation for looking at the state of a
computation, and the *ideal* notation for doing so is the language that the
original program was written in. I think there is great value in being able to
(at least conceptually) interrupt a program at any time, point to any variable
in scope, and print out its value in the same notation that you might enter into
a file or a REPL. (There's some fine print about opaque representations and
constructors that I'll ignore - this is a conceptual discussion.)

What does this capability require? Well, the main thing is that all values must
have an externally comprehensible notation. For (good) technical reasons,
functions usually do not have that capability in most languages, although
exceptions exist. Another tricky thing to handle is *references* - once these
exist, a value is not completely self-contained, but is only meaningful in
relation to some context from which the value cannot be removed. Mutability is
not *necessarily* a problem, e.g., a mutable array is harmless, but if widely
shared in a mutable way, simply looking at the *values* without also considering
*object identity* may give a misleading view of the state of a computation.
*Aliasing* of mutable values is what really adds the complexity here.

To me that is the essence of value-oriented programming: structuring programs as
functions that accept plain values and produce plain values, such that the
behaviour of the program can be understood simply by looking at how the values
float around. It's not a critical problem if some functions are impure, or there
is some global state somewhere, but it should be minimised. My experience is
that most of the benefit of functional programming comes from *this* style of
programming, rather than the more fancy uses of functions. Indeed, I think it is
in fact a lot easier to understand functions where you can look at input and
output, than when they pass around black box functions.

Incidentally, while classic array programming in the style of
[APL](https://en.wikipedia.org/wiki/APL_(programming_language)) also strongly
encourages this style of programming, most (all?) APL dialects use a completely
different notation for printing values than you as a user use when entering
them. In contrast, Futhark uses the same notation for input and output:

```Futhark
> iota 10
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

Compare with APL:

```
> ⍳10
1 2 3 4 5 6 7 8 9 10
```

It is perhaps a minor difference, but a man is entitled to his idiosyncratic pet
peeves. We can be friends until the revolution, and after that it gets
difficult.

## In Futhark

We often call Futhark a functional language, and it really does resemble a
fairly ordinary ML dialect. But Futhark restricts function values in certain key
ways:

* A function may not be returned from a branch.
* A function may not be put in an array.
* A function may not be used as a loop parameter.

These restrictions are ultimately rooted in compilation concerns, but they also
mean that using functions-as-data is discouraged. Instead, Futhark programmers
mainly use functions as a [higher-order organising
principle](2020-05-03-higher-order-parallel-programming.html), where
higher-order functions are used for sometimes fancy *control flow*, but never
for *data*. The language is rich enough to do classic functional programming
party tricks such as [this combinator library for producing
audio](https://github.com/diku-dk/larm) (basically [functional reactive
programming](https://en.wikipedia.org/wiki/Functional_reactive_programming)),
but at *run-time*, all that happens is passing around values that are
understandable within the nomenclature and notation of the Futhark source
language.

From a didactic perspective, I often make use of the value-oriented style of
thinking [when teaching data-parallel
programming](https://github.com/diku-dk/dpp-e2025-pub). It turns out that data
parallel programming has the lovely property that the intermediate states of a
computation can often be described as just a handful of arrays. This is because
this style of programming emphasises operating on the *entire* state at once, in
contrast to classic functional programming, which often emphasises fine-grained
recursion, where the meaning of an intermediate result must be considered in the context of the call stack in which it occurs.

As a demonstration of how bulk data-parallel programming makes it easier to
understand algorithms, consider the problem of removing the negative elements
from this array:

```Futhark
let as = [-1, 2, -3, 4, 5, -6]
```

For each element, see if we want to keep it:

```Futhark
let keep = map (\a -> if a >= 0 then 1 else 0) as
-- [ 0, 1, 0, 1, 1, 0]
```

Then compute the prefix sum of the `keep` array:

```Futhark
let offsets1 = scan (+) 0 keep
-- [ 0, 1, 1, 2, 3, 3]
```

For the elements we wish to keep (i.e., the ones where the corresponding element
in `keep` is 1), this array contains their 1-indexed positions in the final
result. We can turn it into 0-indexed positions simply by subtraction:

```Futhark
let offsets = map (\x -> x - 1) offsets1
-- [-1, 0, 0, 1, 2, 2]
```

We can then construct the final result using a
[`scatter`](../examples/gather-and-scatter.html), which I won't do here. But
note how I was able to explain the process of parallel filtering by showing the
gradual evolution of a single value. [Here is an example of taking the same
approach to explain parallel radix sort](../examples/radix-sort.html).

I should note that I am not uncritically in favour of letting the needs of
teachers dictate how a language evolves - Haskell's has suffered from many of
its stakeholders mainly focusing on the needs of people who use Haskell for a
few months and then never again (students taking a course). However, my
experience is that the value-oriented approach also makes it easier for
experienced programmers to understand their programs.

I should also note that the above is not Futhark-specific. If you see an APL
programmer derive a solution to a problem, they will do *exactly* this.

## The Dijkstra Angle

Whenever we get too fond of our ideas and our tools, it is a good idea to ensure
we are still being critical. When it comes to the fundamentals of computer
science, my favourite Devil's Advocate is Dijkstra, and it is usually possible
to find some writing of his that explains how the things we love might in fact
be bad. For this post, we turn to
[EWD1036](https://www.cs.utexas.edu/~EWD/transcriptions/EWD10xx/EWD1036.html)
(*On the cruelty of really teaching computing science*). The salient part is
this:

> The statement that a given program meets a certain specification amounts to a
> statement about all computations that could take place under control of that
> given program. And since this set of computations is defined by the given
> program, our recent moral says: deal with all computations possible under
> control of a given program by ignoring them and working with the program. We
> must learn to work with program texts while (temporarily) ignoring that they
> admit the interpretation of executable code.

Oof - my example of filtering above may give some intuition of how the algorithm
works, but it certainly focuses on a *specific computation* rather than *all* -
it barely touches on the program at all. In fact, I didn't even show a complete
program; I just added new bindings as I went along.

This reminds me of how I often end up doing proofs of associativity by
exhaustive case analysis, [in contrast to more elegant techniques that focus on
essential
properties](https://byorgey.wordpress.com/2020/02/23/what-would-dijkstra-do-proving-the-associativity-of-min/).

Still, Dijkstra's admonition that we should work on *programs* does seem like it
should be a decent fit for data-parallelism. Let us see if we can think about
parallel filtering using this approach. First, let us define the problem:

> Given an integer array `xs`, produce an array `ys` such that for all `i`, if
> and only if `xs[i]>=0` then there is a `j` such that `ys[j]==xs[i]`.

There are some problems with this definition:

1. I am not being precise about the bounds of `i` and `j`, but to cut down on
   clutter I am just going to follow the convention that we only consider the
   `i` and `j` that are in-bounds relative to how they are used.

2. The definition implicitly assumes that `xs` has no duplicate elements. This
   is not a *big* problem, as one can always obtain this property simply by
   tagging each element with its index.

3. It does not state that the relative ordering of elements in `ys` should be
   the same as in `xs.`

Point 3 is a pretty major flaw, so let us try to refine how `xs` (the input) and
`ys` (the result) are related:

> For all `j`, then `ys[j]==xs[i]` if and only if `xs[i]>=0` and `j` is the size
> of the set `{xs[l]>=0 | l < i}`.

This specification really just says "the position in the output for a
non-negative number `xs[i]` is equal to the number of non-negative numbers
preceding `xs[i]` in `xs`.

Now the problem becomes: for each `xs[i]` where `xs[i]>=0` compute the
corresponding `j`, which we denote `i_j`. Following the definition of `j` and
using the convention of treating truth as `1` and falsehood as `0`, we get:

```
i_j = sum {xs[l]>=0 | l < i}
```

Since the valid `i`s form an array (they are indexes, after all), this is
exactly computing the sum of the prefixes of `xs`, which is just what [an
exclusive scan](https://futhark-lang.org/examples/exclusive-scan.html) does. So
this expression forms the `i_j`s for all the `i`s we care about, as well as
those we do not:

```
exscan (+) 0 (map (\x -> if x >=0 then 1 else 0) xs)
```

From here it is not so difficult to also suss out the need for a `scatter`.

I would be speaking an untruth if I said that I had used this approach to derive
data-parallel algorithms myself - I am a hacker of heart, and I find that my
style of thinking is much more geared towards thinking about what *happens* than
what something *is*. But I cannot say that Dijkstra is wrong, and I think I will
have to try out his approach in the future.
