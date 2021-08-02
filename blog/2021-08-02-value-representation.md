---
title: How Futhark represents values at runtime
description: Futhark doesn't do it how most other functional languages do it.
---

In an almost literal sense, Futhark is an elaborate collection of
smoke and mirrors that lets the programmer think they are writing a
high-level program with polymorphism, higher-order functions, and all
the other bells and whistles.  But *really*, they are actually writing
a first-order monomorphic C program, full of unboxed data and tight
loops.  Nowhere is this more clear than in how Futhark values are
actually represented in memory at runtime.  In this post I will
describe what that representation looks like.  Apart from perhaps
being interesting to other language designers, it is also useful
knowledge for Futhark programmers who wish to ensure that they are
writing efficient code.  Some of the concerns described below also
justify some of Futhark's more annoying restrictions.  Sometimes the
smoke clears or a mirror breaks and you can see the ghastly reality
that normally lies hidden.

Traditionally, functional languages use a uniform value representation
where all values are represented by a pointer to an object on the
heap.  I admire the elegant simplicity of this approach - particularly
how it makes polymorphic functions straightforward to represent in
compiled code.  If everything is just a pointer, you can freely
shuffle those around, without worrying about what they point to!  The
drawbacks are extremely high pressure on the memory allocator,
potentially significant overhead in memory usage, and a loss of
locality.  For some programs, these are not major problems - those
that mainly perform symbolic processing, such as compilers, spend
their time chasing pointers anyway.  But for Futhark, all three are
problematic:

1. Unpredictable dynamic allocation is a poor fit for massive
   parallelism, as the allocator becomes a synchronisation bottleneck.

2. Memory bandwidth is usually the bottleneck in Futhark's problem
   domain, so inefficient data representations directly impacts
   performance.

3. Locality-based optimisations are crucial for exploiting the memory
   hierarchy of modern machines.  Unless we can know for sure that
   neighbouring values in an array are also neighbours in memory (and
   not just pointers to who-knows-where), we don't have a chance of
   doing such optimisations.

Instead, Futhark's value representation is focused on *unboxed*
values, where values of any built-in scalar type such as `i32` are
represented merely as themselves, located on the stack or in
registers.  This has major ramifications for compilation scheme as a
whole - since there is no uniform representation, polymorphic
functions must be *monomorphised*, where a version is generated for
distinct each type the function is used with.  This leads to increases
in code size and prevents separate compilation, but since Futhark is
not aimed at large programs, it is not a high price to pay.  A more
subtle constraint is that it forces us to use values whose total size
can be determined statically, with arrays as the single example of
dynamically-sized values in the languages.  This is in contrast to
most (all?) other functional languages, where you can easily define a
recursive data type whose values can be of arbitrary size at runtime.
For this reason, Futhark does not support recursive data types.

## Scalar types

Futhark's main code generator produces C code, so we can explain the
value representation in terms of the C that is generated.  A Futhark
function that takes an argument of type `f64` will be translated into
a C function that takes an argument of type `double`.  No trickery or
indirection, although we're assuming that the function is not just
inlined entirely.  We do not distinguish signed and unsigned types -
to the compiler, `i32` and `u32` are exactly the same.

Tuples are merely flattened and then represented by their individual
components - there are no *tuple objects* at runtime, or even in our
intermediate representation.  A function that takes an argument of
type `(f64,f64)` will result in a C function that takes two arguments
of type `double`.  This has one performance implication: whenever you
pass a tuple to a function, the *entire* tuple is copied (except any
embedded arrays, which are always passed by reference as we'll see
below).  This is in contrast to most other functional languages, where
tuples are boxed, and so passing them around involves only copying a
pointer.  Due to the compiler's heavy use of inlining, this is rarely
a problem in practice, but it can be a concern when using the `loop`
construct with a large tuple as the loop variant parameter.

Records and sum types are handled by turning them into tuples, and
then processing the tuples as described above.  Records are turned
into tuples by simply sorting their fields and discarding the labels.
The translation of sum types is more tricky and less efficient.  As a
starting point, a sum type is turned into a tuple containing all the
payload components in order, prefixed with an `i8` tag to identify the
constructor.  For example,

```Futhark
#foo i32 bool | #bar i32
```

would be turned into a tuple

```Futhark
(i8, i32, bool, i32)
```

and the value

```Futhark
#foo 42 false
```

would be represented as

```
(1, 42, false, 0)
```

where `#foo` is assigned the tag `1` because it is alphabetically
after `#bar`.

This is obviously fairly inefficient, especially for large types.  In
practice, the compiler does a bit of deduplication as well.  If
multiple constructors have payload elements of the *same* type, we
assign them to the same elements in the result tuple, so the
representation of the above sum type would actually be the following:

```Futhark
(i8, i32, bool)
```

This can still have significant performance implications.  The types
must be the *same* for deduplication to take place - despite `i32` and
`f32` being of the same size, they cannot be assigned the same tuple
element.  What would the element of that tuple be?  This is because
our intermediate representation, for simplicity, does not have a
C-like [union type](https://en.wikipedia.org/wiki/Union_type#C/C++).
Matters are even graver when the payload is an array.  The type


```Futhark
#foo [n]i32 | #bar [n]i32
```

will be efficiently represented as

```Futhark
(u8, [n]i32)
```

but

```Futhark
#foo [n]i32 | #bar [n]f32
```

will become

```Futhark
(u8, [n]i32, [n]f32)
```

which is not great - and this cannot be resolved simply by adding
C-like union types.  Take caution when you use sum types with large
arrays in their payloads.

## Array types

Arrays are the only Futhark values that are boxed - that is, are
stored on the heap.  They are also the only values that have a dynamic
size, but crucially that size (the shape of the array) is known "at
the top", without having to look at the contents of the array - and is
in fact usually known far before the array elements themselves are
computed.  This is not the case for recursive data types as we know
them from mainstream functional languages.  This property is what
allows the Futhark compiler to aggregate, combine, and hoist
individual small allocations out into large bulk allocations, which
avoids pressure on the memory allocator.

The elements of an array are unboxed, stored adjacent to each other in
memory.  This means we have zero memory overhead except for the
minuscule amount needed to track the shape of the array.  Computing an
element address is as efficient as can be: just multiply the index
with the statically known element size.  At the surface language
level, Futhark may appear to support "arrays of arrays", and this is
indeed a convenient aspect of its programming model, but at runtime
multi-dimensional arrays are stored in flattened form.  A value of
type `[x][y]i32` is laid out in memory simply as one array containing
*x\*y* integers.  (I'm skipping over some details here, see the fine
print below.)  This is one reason why Futhark does not allow *jagged*
or *irregular* arrays, such as `[[1,2], [3]]` - while they are
straightforward to handle if arrays could contain pointers to other
arrays, they require [complex supporting machinery such as flag
arrays](https://futhark-book.readthedocs.io/en/latest/irregular-flattening.html)
if you want a completely unboxed representation.

Since arrays cannot contain other arrays, memory management only has
to be concerned with one level of indirection.  In practice, it means
that Futhark can use straightforward reference counting to keep track
of when to free the memory backing an array, as circular references
are not possible.  Further, since arrays tend to be large and
relatively few in number, the usual performance impact of naive
reference counting is not present.

So that's (multi-)dimensional arrays of scalars.  But how does Futhark
represent arrays of tuples, such as `[](i32,bool)`?  One option is the
obvious dense one:

```
0           4      5           9      10
|     i32   | bool |     i32   | bool |...
```

This is conceptually simple, but has major downsides.  The main one is
that the second `i32` value will start 5 bytes into the array - likely
an [unaligned
address](https://www.cs.umd.edu/~meesh/cmsc411/website/projects/outer/memory/align.htm).
In practice, almost no languages do it this way.  Even C will normally
insert padding bytes in structs to ensure alignment, which might lead
to representations such as this one:

```
0           4            8           12           16
|     i32   |    bool    |     i32   |    bool    |...
```

Now every integer is located on an address divisible by four, but with
3 bytes of useless padding for every element - a substantial overhead.
Further, this representation makes it expensive to do *projection*.
What if some part of our program only needs the Boolean parts?  Each
memory transaction will fetch an entire consecutive chunk of memory
(say, a cache line) and if the chunk contains data we do not need,
then we are wasting precious memory bandwidth.  This is a common
problem in high performance computing, and the usual solution is to
use a so-called [structure of
arrays](https://en.wikipedia.org/wiki/AoS_and_SoA) representation.
Instead of representing arrays of structs (or in our case, tuples), we
maintain *a distinct array for every component of the element tuples*:


```
0           4            8           12           16
|     i32   |     i32    |     i32   |     i32    |...

0      1      2      3
| bool | bool | bool | ...
```

This gives us two dense, properly aligned arrays of primitives.  And
if we only need the booleans, then the code needs only traverse that
array.  In Futhark terms, an array `[n](a,b,c)` is at runtime
represented as the tuple `([n]a,[n]b,[n]c)`.  This has some
significant implications.  For example, `zip` and `unzip` are very
cheap, as the actual runtime representation is in always "unzipped",
so these functions don't actually have to do anything.  This
representation was put in place in the very early days of the Futhark
compiler ([before it was even called
Futhark](https://futhark-lang.org/blog/2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html#early-days))
and has served us very well.

This desire to ultimately deal only with arrays of scalars is why we
use such a relatively inefficient encoding of sum types.  By turning
everything into tuples, and then arrays of tuples into arrays of
scalars, we end up with extremely simple memory layouts.  It's not yet
clear to us how to improve sum types without significant cost (whether
in complexity or performance) elsewhere.

Finally, I'll mention that the representation above is not merely the
memory layout (and in fact, see below), but also the value model used
throughout our intermediate representation.  Monomorphisation,
converting records and sum types, and turning arrays of tuples into
tuples of arrays is done as one of the first steps of our compilation
pipeline.  The various optimisation passes do *not* deal with tuples
at all - just scalars and arrays of scalars.  I think this simplicity
is key to why we've been able to implement such aggressive
optimisations.

## The fine print

I couldn't decide whether the following details were significant or
not, but they impeded the narrative of the main body of text, so to
the bottom they went.

While functional compilers for languages such as OCaml and Haskell do
have the notion of a uniform representation, they also put effort into
unboxing intermediate results.  If you write `(x+y)*z` in Haskell,
it's not actually going to heap-allocate the result of `x+y` just to
immediately read it afterwards.  (Well, depending on whether you're
adding together values of particularly fancy types.)  For strict
languages this kind of local unboxing is not so difficult, although
Haskell being lazy also has to perform strictness analysis to ensure
that it is not compromising on laziness, as
[thunks](https://wiki.haskell.org/Thunk) *must* be boxed.  Most of
these languages also provide some support for explicitly working with
unboxed data ([GHC even has polymorphism for unboxed
values](https://www.microsoft.com/en-us/research/publication/kinds-are-calling-conventions/)).

Futhark isn't the only functional compiler to take the monomorphic
whole-program optimising approach.  [MLton](http://www.mlton.org/) is
a production-ready compiler for Standard ML that also does it this
way, and while the compile times are certainly very long by most
standards, they are not unusably so - for example, MLton can compile
the 100k line [MLKit](https://elsman.com/mlkit/) in about 20 minutes.

While the description of arrays above is correct in that array
elements are stored unboxed, I was intentionally vague about their
exact layout.  This is because the Futhark compiler is at liberty to
arrange the precise element order in whatever way it wishes - whether
[row or
column-major](https://en.wikipedia.org/wiki/Row-_and_column-major_order),
or a mixture, depending on what it decides is most efficient given how
the array is eventually used.  It may even decide to duplicate an
array and store it twice in two different layouts, in case it is
simultaneously traversed in two different ways that cannot be
efficiently handled with a single layout.  Slicing operations such as
`A[:,0]` (which extracts the first column of an array) may also lead
to elements that are not consecutive in memory at all - again, it
depends on how the compiler decided `A` should be stored in memory,
and whether it decides to make the slice a copy rather than merely a
delayed "view".

Take care not to confuse the precise in-memory layout (which is an
implementation detail) with the representation used for interacting
with Futhark arrays via the [C
API](https://futhark.readthedocs.io/en/latest/c-api.html), which
always uses logical row-major ordering.
