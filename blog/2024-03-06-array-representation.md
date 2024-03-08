---
title: Evolution of Futhark's Array Representation
author: Troels Henriksen
description: How the compiler represents the most important data structure in the language.
---

As a functional array language with a focus on compiler optimisations,
it is no surprise that much development has gone into Futhark's
representation of arrays. After recently spending some time on
tweaking it yet again, I figured it might be interesting to write a
few words about how it has changed over time. We also have a [paper on
the basic approach](../publications/ifl22.pdf)
and a [paper on a fancy optimisation built on top of
it](../publications/sc22-mem.pdf) for those who
prefer PDFs.

## Array representations generally

Before getting to compiler representations, let us talk more generally
about how to store arrays on a computer. For simplicity, all of the
examples will use one- or two-dimensional arrays, but the ideas
generalise to any rank.

Arrays are conceptually simple: they are sequences of values. If we
want to get fancy, we can allow those elements to be arrays
themselves, yielding multidimensional arrays, and if we want to get
*really* fancy, we can allow each of those subarrays to be of a
different size. But now we have gotten too fancy, so let us dial back
the discussion to be about multidimensional *regular* arrays
(sometimes also called *rectangular*), where all elements have their
own size. In fact, we will only consider multidimensional arrays of
primitive types, as arrays of records and other such compound types
can be [transformed
away](2021-08-02-value-representation.html#array-types).

How do we represent such arrays at runtime in a computer? The simplest
solution I know of is to store the shape of the array as a *shape
vector*, one integer per dimension, as well as a pointer to somewhere
in memory where all the actual array elements are stored.

There are many ways in which we can store the elements. The crucial
question we need to answer is how we map a point in the (potentially)
multidimensional *index space* of the array to the one-dimensional
*offset space* of computer memory. The order in which we store the
elements can have a drastic impact on performance due to spatial
locality effects (e.g. cache efficiency). For example, consider
storing the elements of following two-dimensional array:

```
[[1,2,3],
 [4,5,6]]
```

If we use the common *row-major order*, then the elements are laid out
in memory as

```
1 2 3 4 5 6
```

and we can transform a two-dimensional index `(i,j)` to a flat
offset with the formula `i*3+j`.

Each row is contiguous in memory, meaning it is efficient to traverse
a row sequentially. However, if we traverse the first *column*, we see
that the values `1` and `4` are distant in memory (well, not so
distant for this small array). On the other hand, the *column-major
order* representation would use the ordering

```
1 4 2 5 3 6
```

and the indexing formula becomes `j*2+i`. Now columns are contiguous
in memory and can be traversed efficiently, but traversing a row
involves jumping with a larger *stride*. With a simplistic
representation, consisting of just a shape vector and a pointer, which
layout should we pick?

It is important that array layout and array usage are a good
match. Futhark does not provide direct control over memory layout to
the programmer, but it is crucial that the compiler *itself* is able
to transform array layouts as part of its optimisations. Further,
different arrays might need different layouts. This means the compiler
needs to have some kind of language, or notation, in which it can
reason abstractly about array layouts, and annotate the program
representation with how arrays should be laid out in memory at
run-time.

Apart from performance concerns, the array representation also affects
which operations can be performed efficiently at run-time. For
example, in the representation outlined above, where the only metadata
is the shape of the array and we store the elements in row-major
order, it is cheap to interpret an array of 10 elements as a
two-dimensional array of shape `[2][5]` (using [Futhark shape
notation](2016-12-09-two-syntax-design-problems.html#array-types)),
simply by changing the shape vector and not touching the elements.
This means that we could guarantee that a *reshape* operation is
essentially free in all cases. On the other hand, consider *slicing* a
column out of a two-dimensional array in order to obtain a
one-dimensional array. Since this array representation requires
contiguous storage of elements, and the elements of a column are not
contiguous, we have to allocate new space and copy over the elements.

It seems desirable to come up with an array representation that
minimises how often a transformation requires us to copy array
elements, but as we will see, flexible representations have their own
downsides. As we will see, the Futhark compiler actually moved from an
initially very flexible representation to a rather simple one.

## Index functions

We will stick to a model in which a *q*-dimensional array consists of
a pointer *p* to somewhere in memory, along with an *index function*
*f*

    f : ℕ^q → ℕ

that maps an index in a *q*-dimensional index space (the domain of the
function) to a flat offset relative to the pointer. That is, when
indexing an array *A[I]* we compute the address *p+f(I)* and fetch an
element from there (probably multiplying by an element size along the
way). In some treatments, index functions are functions from indexes
to *values*, but in Futhark we treat them as producing *offsets* -
this is because we want to use them to reason about the physical
storage of data, not just to model arrays theoretically.

We've already made an important restriction: arrays are associated
with a *single* memory block, which means we cannot guarantee
efficient (zero-copy) concatenation of arrays. This is to make memory
management easier.

As mentioned above, the Futhark compiler needs the ability to
transform and reason about index functions, in order to make decisions
about array layout. This means we cannot allow index functions to be
completely arbitrary functions.

In our original implementation, an index function started with a
"base", indicating the shape of an underlying row-major array:

```
RowMajor(n,m)
```

For any index function, we must be able to apply it to any point in
its index space:

```
apply(RowMajor(n,m), (i,j)) = i * m + j
```

Note that the elements `n,m` of the index function, or the indexes
`i,j`, do not need to be numbers - they can be symbolic variables
instead, such that `index` produces an *expression* rather than a
number. We can then embed index functions directly into the
intermediate representation (IR) that the compiler uses to represent
Futhark programs. Whenever we bind an array (here a copy of an
existing array `A` of type `[n][m]i32`) to a name `B`, the IR contains
the type of the array, the memory block in which it is stored
(skipping some detail here), and the index function used to access it:

```
let (B : [n][m]i32 @ B_mem -> RowMajor(n,m)) = copy A
```

When the compiler generates code for an array access `B[i,j]`, it
symbolically applies the index function `RowMajor(n,m)` to the index
`(i,j)`, yielding the expression `i * m + j`, which will then be
emitted as code. At runtime, the concrete values of `i,m,j` will be
known, and the actual offset will be computed. This means that *index
functions do not exist at runtime* - they are entirely a compile-time
mechanism that the compiler uses to track array layouts.

Index space transformations of an array, such as slicing or indexing,
are done by adding more operations to the index function, which
essentially consts of an arbitrary-length sequence of transformations.
For example, if *F* is an index function for a two-dimensional array,
we can index the first dimension with

```
F->Index(i)
```

yielding a one-dimensional index function. We define application of
the index function thus:

```
apply(F->Index(i), I) = apply(F, (i, I))
```

(Writing `(i, I)` for prefixing `i` to the tuple `I`.)

In the IR, we might use it like this:

```
let (C: [m]i32 @ B_mem -> RowMajor(n,m)->Index(i)) = B[i]
```

Note how the array `C` is stored in the same memory block `B_mem` as
the array `B` - this directly expresses the fact that a copy is not
necessary. In fact, the expression `B[i]` does not result in the
compiler generating any code at all - when `C` is indexed at some
point, the compiler will consult the index function, which is
statically known, and emit the appropriate address calculations.

There is in principle no limit to which kinds of index space
transformations we can support in this way, as long as we can define
application for them, and the Futhark compiler used a robust set of
them - for example including things like `Transpose`, which was how we
represented column-major arrays:

```
apply(F->Transpose, (i,j)) = apply(F, (j,i))
```

In some cases, we have no choice how to assign an index function to an
array - the result of an index expression must certainly be an `Index`
on top of the underlying index function, as above, and a transposition
must of course be a `Transpose`. But in those cases where we are
creating a *fresh* array (such as when doing a `copy`, or for that
matter a `map`), the compiler can invent whatever index function it
wishes.

## Reconciling different index functions

Since index functions are treated as statically known symbolic
functions by the compiler, questions arise about what happens in cases
where the index function *cannot* be fully statically known. The
compiler can always fall back to *normalising* index functions to be
e.g. row-major, at the run-time cost of a copy, but naturally we don't
want to do that all the time.

Consider an array `A` with index function `RowMajor(n,n)`. As above,
an expression `A[i]` produces an array with index function
`RowMajor(n,n)->Index(i)`, and similarly `A[j]` produces
`RowMajor(n,n)->Index(j)`. If we have an expression

```
let B = if c then A[i] else A[j]
```

then what is the index function of `B`? We can avoid a copy by
augmenting the `if` to also return an integer that is used in the
index function of `B`, as follows:

```
let (l: i64, B : [n]t @ B_mem -> RowMajor(n,n)->Index(l)) =
  if c then (i, A[i])
       else (j, A[j])
```

This is essentially piggybacking on the machinery that we already use
to handle expressions that return arrays of unknown ("existential")
size
([previously](2023-05-12-size-type-challenges.html),
[previously](2021-10-16-explicit-existentials.html)).

Unfortunately, the only part of the index function that we can
parameterise are the scalar "leaves" (here, the specific argument to
`Index`). The structure itself cannot be parameterised. This
limitation shows up in cases such as

```
let B = if c then A else transpose A
```

The two branch cases produce index functions `RowMajor(n,n)` and
`RowMajor(n,n)->Transpose` respectively - how can we find a [most
general
unifier](https://en.wikipedia.org/wiki/Unification_(computer_science)#Syntactic_unification_of_first-order_terms)
that can be instantiated with either? We could perhaps extend our
notion of parameterisation of index functions to allow something like

```
let (f: IxFun, B: f) = ...
```

where `f` is then a *run-time representation* of an index function,
but this radically complicates the run-time behaviour of Futhark
programs, which we would rather avoid. Instead, our solution is that
whenever the index functions differ structurally, we insert a copy to
normalise:

```
let (B_mem: mem) (B: [n][n]t -> B_mem @ RowMajor(n,n)) =
  if c then copy A
       else copy (transpose A)
```

We can also parameterise over the memory block here to avoid having to
copy in the first branch, but w'll focus on index functions for this
post.

Incidentally, such a copy is also inserted when an array with a
non-`RowMajor` index function is returned from an entry point, because
we require that all arrays passed in or out of Futhark are in
row-major order, as otherwise the external representation would need
to somehow encode the index function.

## Recognising array layouts

Index functions only address the problem of computing the offset for a
single element, but sometimes the compiler wishes to generate code for
bulk operations, where taking a more high level view can be more
efficient.

Suppose we have an array with index function `F1`, and we want to copy
its elements to form a new array with index function `F2`. We assume
that the two index functions have the same index space, as otherwise
such a copy is nonsensical. One obvious way to implement the copy is
to simply iterate through all indexes `I` and copy from offset `F1(I)`
to offset `F2(I)` in the respective memory blocks. While this can be
done in parallel, it is most likely not maximally efficient. If the
two index functions represent the same element order, and they are
contiguous, then the copy can be done as a more efficient
`memcpy`-like bulk copy, which might take advantage of
hardware-specific optimisations. Similarly, when copying from a
row-major to a column-major index function, it is *much* more
efficient to make use of a locality-optimised implementation of
transposition than it is to use a naive loop (even if parallel). These
are special cases, but they are rather common, and so it is crucial
that we are able to reliably detect them. This is where a very
flexible index function representation becomes awkward, as it can be
difficult to analyse long chains of transformations and recognise
known structures.

The Futhark compiler largely relied on pattern-matching index
functions to detect these important special cases for copies, but it
turned out to be unviable when we set out to implement more
complicated access pattern analyses, such as needed for Philip
Munksgaard's work on [short
circuiting](2022-11-03-short-circuiting.html).
We needed a simpler and more uniform representation of index functions
than the purely structural one.

## Introducing LMADs

Linear Memory Address Descriptors (LMADs) were introduced by Paek,
Hoeflinger, and Padua in their paper [Efficient and Precise Array
Access Analysis](https://dl.acm.org/doi/pdf/10.1145/509705.509708)
from 2002, where it was used for analysing the access patterns of
imperative loops for the purpose of automatic parallelisation. Some
years ago, Futhark co-creator Cosmin Oancea realised that LMADs might
also be useful as a uniform representation of index functions, since
they come with robust prior work on how to analyse them.

LMADs are quite simple. An LMAD for a *q*-dimensional array consists
of an integral offset `o` and *q* pairs `(n,s)`, where `n` is the
non-negative *size* of the dimension and `s` is the integral *stride*.
The pairs are conventionally written juxtaposed, as:

```
o + (n1,s1)(n2,s2)...(nN,sN)
```

To apply an LMAD to a *q*-dimensional index, we simply multiply each
index component with the corresponding stride, sum the resulting
terms, and add the offset. For example, an array of shape `[2][3]` in
column-major order and zero offset would be written as the LMAD

```
0 + (2,3)(3,1)
```

and application to an index `(i,j)` is done by computing

```
i*3 + j*1 + 0
```

An LMAD can represent many (but not all) of the typical index space
transformations. For sample, we can *fix* any given dimension
("indexing") by multiplying the index with the corresponding stride
and adding it to the offset. For example, if we have a two-dimensional
array `A` with LMAD

```
o + (n1,s1)(n2,s2)
```

then the LMAD of the array `A[i]` would be

```
(o + i*s1) + (n2,s2)
```

where the parentheses simply serve to clarify that the offset is
separate from the pairs. Transposition (or any permutation of
dimensions) is done simply by permuting the LMAD. Negative strides are
useful for expressing reversals. The array `reverse A` (where the
outermost dimension is reversed) would be given by this LMAD:

```
(o + (n1-1)*s1) + (n1,-s1)(n2,s2)
```

While LMADs do in principle allow zero strides, we do not provide any
mechanism for creating them in Futhark, as they have unfortunate
implications for [in-place
updates](2022-06-13-uniqueness-types.html) - generally, we do not
allow dimensions to overlap, although this is in principle allowed by
the LMAD representation.

LMADs have strong similarities to [dope
vectors](https://en.wikipedia.org/wiki/Dope_vector) (and for all I
know might have been used under a different names previously), but
with the common difference that the stride of a dimension does not
necessarily imply anything about the innermost dimensions. One
attractive quality is that any LMAD for a *q*-dimensional array, no
matter how complicated, can be represented with *2q+1* integers. This
also makes it easy to parameterise them, because those integers are
all there is to parameterise. For any *q*, the structure is completely
fixed (and Futhark does not allow rank polymorphism so *q* is always
known).

Using LMADs, the `if`-and-`transpose` example above can now be solved
as follows:

```
let (s1: i64, s2: i64, B_mem: mem) (B: [n][n]t -> B_mem @ 0 + (n,s1)(n,s2)) =
  if c then (n, 1, A)
       else (1, n, transpose A)
```

The two branches now return the appropriate strides that are then
referenced in the LMAD `0 + (n,s1)(n,s2)`. No copies! The copying
special cases can also be recognised fairly easily at run-time simply
by inspecting the concrete values of the LMADs, since they are
structurally similar.

## Multiple LMADs

Unfortunately, not all layouts can be expressed with LMADs. In
particular, the restriction to a uniform stride along each dimension
means some index space transformations are inexpressible. For example,
consider first transposing a matrix, then flattening it to a single
dimension: `flatten (transpose A)`.  If the original value of `A` is

```
[[1,2,3],
 [4,5,6]]
```

and if we suppose it is stored in memory in row-major order as

```
[1,2,3,4,5,6]
```

then semantically the transformed value should be

```
[1,4,2,5,3,6]
```

but there is no way to write a one-dimensional LMAD that produces the
necessary offsets.

However, this can be handled if we use an index function
representation that allowed a *composition of multiple LMADs*.

The way we index a composition of two LMADs `L1∘L2` is by first
applying the index to `L2`, yielding a flat offset, *unravelling* that
offset into the index space of `L1`, then applying `L1` to that now
multi-dimensional index. In particular, `L1` and `L2` need not agree
on how many dimensions exist in their index space. The cost of the
unravelling is somewhat significant (a division, modulo, and
subtraction per dimension), although probably still less than the cost
of a memory access, except for very high-dimensional arrays.

We used the composition-of-LMADs representation for a few years, as we
were reluctant to to perform copies when `flatten` (and `unflatten`)
were combined with transpositions, but we eventually realised that we
still had many of the problems caused by our initial structural index
functions. In practice, the compiler would throw up its hands and give
up on analysing any index function that comprised more than a single
LMAD, and the compiler had lots of hacks and ad-hoc code to detect the
cases when a single LMAD would be sufficient. Eventually we decided
that a few extra copies was a small price to pay in return for
significantly simplifying the representation, and so we [finally moved
to single-LMAD index
functions](https://github.com/diku-dk/futhark/pull/1982). Our journey
from complexity to simplicity had left a little more representational
scar tissue that I haven't gotten into, which we
[gradually](https://github.com/diku-dk/futhark/pull/2083) removed over
a few [further pull
requests](https://github.com/diku-dk/futhark/pull/1986), and were
finally left with [an index function being a single LMADs, with no
additional information](https://github.com/diku-dk/futhark/pull/2085)

The end result was a significant simplification of the compiler - both
conceptually and technically - and with no performance regressions
outside of contrived test programs.

One reason we could get away with this is that index functions are not
the *only* way in which we optimise array accesses. If you write
something like `let B = flatten (transpose A)`, then *yes*, that will
cause a copy *if B is not optimised away entirely*. There are many
other places in the compiler where operations on *B* can be fused,
simplified, inlined, or otherwise adjusted to directly operate on `A`
instead. Although when specifying worst case behaviour in the cost
model, we must of course [not promise more than we can
guarantee](2022-01-27-cost-models-are-contracts.html),
so reshaping now in theory a copying operation - which is a bit
unusual among array languages.
