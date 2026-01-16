---
title: Are arrays functions?
description: Sometimes two things are the same if you are sufficiently close or sufficiently far away, but different at a moderate distance.
---

When I was a youngster first perusing the [Haskell documentation for
arrays](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array.html),
I was amused to find the following description of just what these mysterious
things might be:

> Haskell provides indexable arrays, which may be thought of as functions whose
> domains are isomorphic to contiguous subsets of the integers.

I found this to be a hilariously obtuse and unnecessarily formalist description
of a common data structure. Now, older, wiser, and well ensconced in the ivory
towers of academia, I look at this description and think that it is actually a
wonderful definition of the essence of arrays! And given that this sentence
still lingers in my thoughts so many years later, who can say that it is not
actually a far better piece of documentation than some more prosaic description
might have been?

To a language designer, the correspondence between arrays and functions (for it
*does* exist, independent of whether you think it is a useful way to document
them) is alluring, for one of the best ways to improve a language is to make it
smaller. Our goal is not to unify the *representation* of arrays and functions,
of course - nobody would seriously claim that representing an array via some
[Church-encoding](https://en.wikipedia.org/wiki/Church_encoding) is a good idea
in a supposedly practical programming language. Instead, what might be
worthwhile considering is what consequences might arise from unifying arrays and
functions at the syntax or type level, and why Futhark ultimately has not done
so.

There is some prior work to consider. The array language
[K](https://johnearnest.github.io/ok/index.html) has a syntactic unification of
arrays and functions, as both are indexed/applied with the notation `f[x]`. This
is however pretty much where the correspondence stops. As an APL derivative, K
programming is based on bulk operations on entire arrays, rather than
element-at-a-time programming, and the operators that perform these bulk
operations cannot be applied to functions. And of course, K [has no type
system](https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/),
so the correspondence is purely syntactic.

[Dex](https://github.com/google-research/dex-lang) is a research language
[previously covered on this channel](2020-12-28-futhark-and-dex.html), which
also leverages the array-function correspondence, although mostly at the
conceptual level such that they *feel* similar. As a starting point, a function
from `a` to `b` in Dex uses the conventional notation `a -> b`, while an array
with index type `a` and element type `b` is written `a => b`. It is required
that `a` is a type that is isomorphic to a contiguous subset of the integers,
and hence an array type in Dex can really be thought of as a precomputed and
efficiently represented function. Anonymous functions are written as `\x->e`,
while arrays are constructed as `for x.e`. Arrays are transformed using a
"pointed" style, using explicit indexing, similar to how functions are defined
with named parameters that are then passed to other functions.

Many of the common function operations have a nice interpretation for arrays as
well. For example, currying/uncurrying is equivalent to unflattening/flattening
an array - consider how currying `(a,b) -> c` to `a -> b -> c` really is the
same as going from `(a,b) => c` to `a => b => c`. Partial application is like
fixing a dimension. Flipping the parameters of a function is like transposing an
array. Composition is like applying a permutation array to another. It is very
interesting to me how this line of thinking encourages recognising common
patterns and interpreting them differently. It is particularly interesting
because arrays and functions fundamentally are completely different types in
Dex, with few facilities provided for using them via a common abstraction (e.g.
there is no `transpose` function that works for both), but merely through
suggestive syntax and *feel* is the programmer encouraged to think in different
ways.

Now let us consider to which extent a unification of arrays and functions might
be viable in Futhark. First, there is no hope of unification at the type level.
To allow for efficient [defunctionalisation](../publications/tfp18.pdf), Futhark
imposes restrictions on how functions can be used; for example banning returning
them from branches. These restrictions are not (and ought not be!) imposed on
arrays, and so unification is not possible. Also, in Futhark an array type such
as `[n]f64` explicitly indicates its size (and consequently the valid indices),
which can [even be extracted at
run time](2025-09-26-the-biggest-semantic-mess.html). This is not possible with
functions, and making it possible requires us to move further towards dependent
types - which may of course be a good idea anyway.

On to syntax. It would be no great challenge to replace `a[i]` with `a i`. While
it looks strange to me, any sort of change to notation looks strange initially,
and so it is not something I will dwell on overmuch. The main challenge is
actually *slicing*. Futhark supports a fairly conventional Python-like notation
for array slices, namely `a[i:j]`. This does not have such a simple
correspondence with function application syntax. One solution would be to allow
the application of an array to an entire *index array*, rather than just a
single index, producing an array of the same shape. That is, the application `a
[i, j, k]` would be equivalent to `[a[i], a[j], a[k]]`. Since Futhark already
has a decent notation for constructing ranges, this would allow the slice
`a[i:k]` to be written `a(i..<k)` (the parentheses solely for precedence
reasons). Of course, this could also be allowed using the existing bracket
syntax, merely by allowing `a[i]` where `i` is an array.

The operational guarantees are a little trickier to wrangle. Currently, slicing
is guaranteed to be an essentially free operation that merely fiddles with the
[array metadata](2024-03-06-array-representation.html). However, this cannot be
guaranteed when slicing with an arbitrary indexing array, as there may be no
expressible pattern to the indices, which may contain duplicates, arbitrary
holes, etc - in fact, it fully generalises filtering and expansion. The compiler
would have to put in significant work to detect and exploit the efficient cases
corresponding to standard slices, and such reverse-engineering of programmer
intent is [antithetical to the Futhark
philosophy](2022-04-04-futhark-is-a-low-level-language.html). We would much
rather exploit what the programmer has actually *stated*, rather than try to
read between the lines for what they might *mean*.

While I do not think that Futhark is the right language in which to do the
experiment, I would like to see what it would be like for a language to fully
exploit the array-function correspondence. I do not think the best way to do
this is to have only a single type, as the performance implications of the
choice of representation are too dramatic to be left to a compiler. Rather, I
imagine a language that allows shared abstractions that work for both arrays and
appropriate functions. One starting point could be the observation that `a -> b`
and the array type `a => b` are both [functors in the Haskell
sense](https://wiki.haskell.org/index.php?title=Functor), with element type `b`,
meaning they support a "functorial map" (`fmap`) operation. When the parameter
type of a function is isomorphic to a contiguous subset of the integers, then it
is also easy to define [scan and reduction
operations](../examples/scan-reduce.html). We can then start defining functions
that operate on anything "array-like". It is also conceivable that the idea
behind [AUTOMAP](2024-06-17-automap.html) could be extended to "AUTOFMAP", which
would allow operations such as `f + g` when `f` and `g` are functions with the
same domain - mirroring normal mathematical conventions. Other things also
become possible - I do not yet know when it might be useful to perform a matrix
multiplication of functions, but I'd certainly like someone to figure it out and
tell me about it.
