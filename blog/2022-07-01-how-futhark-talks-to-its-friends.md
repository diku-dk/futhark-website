---
title: How Futhark talks to its friends
author: Troels Henriksen
description: Subtle design points in the Futhark C API.
---

Given the title of this post, you may assume that it's about compiler
messages.  But consider that the compiler is completely silent until
it is displeased by something, upon which it will explain its
displeasure in great detail, and simply repeat verbatim if asked for
clarification.  Personally, this is not how I talk to my friends.

Instead this post is about how Futhark programs exchange data with
other programs.  More specifically, we'll look at a particular corner
of the [C API](https://futhark.readthedocs.io/en/latest/c-api.html),
based on some extensions we recently made.

First the basics: Futhark is a purely functional programming language.
It does not have side effects, meaning that programs can't actually
*do* anything.  They have to be passed values from some outside world
(say, the one we live in), and can return some new value.  In practice
this means they are always used as a sub-component of a larger
program.  How data is passed in and out of the Futhark world is quite
crucial.

When a Futhark program is compiled, it turns into a C program along a
header file that describes an API for calling the program. (It can
also [be compiled to Python](2016-04-15-futhark-and-pyopencl.html),
but let's not go there today.)  This is not because C is a wonderful
application language that is desperately in need of a way to write
high performance code, but because C is a useful least common
denominator for APIs. Almost all languages can call C functions, which
makes C APIs very accessible - and they can always be given
language-specific idiomatic wrappers (such as
[genfut](https://github.com/Erk-/genfut) for Rust or
[futhask](https://gitlab.com/Gusten_Isfeldt/futhask) for Haskell).

While Futhark's C API can be used manually in a C program, it is
specifically designed to be easy to use through FFIs.  For example,
all the types it uses are either either builtins of known size
(e.g. `int64_t`), or pointers to opaque structs with hidden
definitions.  This means that other languages don't have to figure out
the sizes of C structs in order to allocate memory.

## Basics

Let's look at some of the implications.  A Futhark function that wants
to be callable from the outside world is called an *entry point* and
defined with `entry`:

```Futhark
entry f (x: i32) (y: i32) = x + y
```

Every entry point turns into a corresponding C function.  In this
case, it will have this prototype:

```C
int futhark_entry_f(struct futhark_context *ctx,
                    int32_t *out0,
                    const int32_t in0, const int32_t in1);
```

Pretty straightforward.  The `futhark_context` struct contains various
internal state and bookkeeping information.  It's not particularly
relevant for what we'll look at, so just ignore it.  The return value
of the function is an error code, as is C tradition.  The actual
result of the Futhark function is written to the object pointed to by
the `out0` parameter.

## Opaque types

The above is a Futhark function that takes two parameters.  Now
consider if we wrote it like this instead:

```Futhark
entry f (x: i32, y: i32) = x + y
```

This is a function that takes a single parameter that just happens to
be a tuple.  How do generate an equivalent C prototype?  The problem
is that C has no built-in tuple type, so it's not obvious what type
the parameter should have.  The Futhark compiler will invent a new
struct type (without exposing its definition!) and use that:

```C
int futhark_entry_f(struct futhark_context *ctx,
                    int32_t *out0,
                    const struct futhark_opaque_6c224689 *in0);
```

Some may feel that `futhark_opaque_6c224689` is not a great name for
the C type representing `(i32,i32)`.  And it probably isn't, as it's a
crude hash of the real name, as C doesn't allow parentheses and other
interesting characters in type names.  The struct is opaque because we
want to be at liberty to change the [value
representation](https://futhark.readthedocs.io/en/latest/performance.html#value-representation)
without breaking API compatibility.  I don't know how many people are
aware that in C you can *declare* a struct without *defining* its
fields, and then freely use pointers to it.  It's a handy technique
for decoupled data hiding, although at the cost of needing more heap
allocation.

If we want a better name, we can use a type abbreviation to come up
with a C-compatible name, and then use it as a type ascription on the
parameter:

```Futhark
type pair = (i32, i32)
entry f ((x,y): pair) = x + y
```

This results in this C function:

```C
int futhark_entry_f(struct futhark_context *ctx,
                    int32_t *out0,
                    const struct futhark_opaque_pair *in0);
```

The general idea is that we inspect the user-provided type ascriptions
in order to figure out what to call things at the C level.  This can
lead to some strange behaviour, as while Futhark is structurally
typed, C is nominally typed.  We may end up in a situation where we
create different (and thus incompatible) C types representing the same
Futhark type.  For example:

```Futhark
type foo = (i32, i32)
type bar = (i32, i32)
entry f (x: foo) (y: bar) = x.0 + x.1 + y.0 + y.1
```

Leading to this C prototype:

```C
int futhark_entry_f(struct futhark_context *ctx,
                    int32_t *out0,
                    const struct futhark_opaque_foo *in0,
                    const struct futhark_opaque_bar *in1);
```

The workaround is *don't do that in the first place*.  But perhaps we
should have provided dedicated syntax for spelling out the desired
C-level interface for entry points, rather than defining subtle rules
based on normally semantics-less type ascriptions.

One example of unintuitive behaviour is that using type abbreviations
only has an effect in cases where the *real* underlying type does not
have a standard representation in C.  For example:

```Futhark
type foo = i32
type bar = i32
entry f (x: foo) : bar = x
```

Which produces this C:

```C
int futhark_entry_f(struct futhark_context *ctx,
                    int32_t *out0,
                    const int32_t in0);
```

It is always possible to make an opaque variant of a type just by
wrapping it in a record or sum-type constructor.  Still, I think this
inconsistency is a design wart.  Unfortunately the C API is one of the
few parts of Futhark that we try hard to keep stable over long periods
of time, so I don't think this will change.

## Peeking through opaque objects

As mentioned before, an important aspect of the design has been making
sure that we do not expose more detail to C than necessary, as we want
the freedom to change object representations transparently.  This also
goes for arrays.  An array of Futhark type `[]f32` becomes a pointer
to a C struct of type `futhark_f32_1d`.  However, while we don't want
to reveal anything about what these structs may look like on the
inside (in particular where their contents are physically stored), we
*do* want the programmer to read their contents into a standard C
buffer:

```C
int futhark_values_f32_1d(struct futhark_context *ctx,
                          struct futhark_f32_1d *arr,
                          float *data);
```

Or to construct them from raw C data:

```C
struct futhark_f32_1d *futhark_new_f32_1d(struct futhark_context *ctx,
                                          const float *data,
                                          int64_t dim0);
```

There is really nothing fancy going on here.  While we keep the
specific representation opaque, we don't mind admitting that arrays
semantically consist of a bunch of values, and that we can convert
back and forth.

But until recently, we didn't make any such admissions for tuples and
records.  If you had a program that accepted a tuple, such as the one
from before:

```Futhark
type pair = (i32, i32)
entry f ((x,y): pair) = x + y
```

Then it was on you to also provide other entry points that allow the
construction of a `pair` value using transparent C values.  For example:

```Futhark
entry mk_pair x y : pair = (x,y)
```

Giving rise to this C prototype:

```C
int futhark_entry_mk_pair(struct futhark_context *ctx,
                          struct futhark_opaque_pair **out0,
                          const int32_t in0,
                          const int32_t in1);
```

Similar entry points can be defined for projecting (extracting) the
elements of the tuple.

Although certainly an application of the principle of data hiding,
this data was perhaps *too* well hidden.  There isn't really much gain
in trying to pretend that tuples are not, well, tuples.  It just leads
to lots of boilerplate.  And worse, it makes it impossible for
languages that *do* have tuples to transparently translate
Futhark-level tuples, exposed as opaque C objects, into native tuples.
How is it supposed to know which entry points correspond to
construction and projection functions?

Therefore, we recently made the Futhark compiler always generate
constructor and projection functions for tuples and records.  For the
`pair` example, the following C functions are available:

```C
int futhark_new_opaque_pair(struct futhark_context *ctx,
                            struct futhark_opaque_pair **out,
                            const int32_t v0,
                            const int32_t v1);

int futhark_project_opaque_pair_0(struct futhark_context *ctx,
                                  int32_t *out,
                                  const struct futhark_opaque_pair *obj);

int futhark_project_opaque_pair_1(struct futhark_context *ctx,
                                  int32_t *out,
                                  const struct futhark_opaque_pair *obj);
```

These functions cannot do anything that hand-written entry points
cannot do, but they are guaranteed to have specific semantics (among
others, they are *really cheap*), which makes them more useful for
tooling.  They are also listed in the
[manifest](https://futhark.readthedocs.io/en/latest/c-api.html#manifest);
a machine-readable description of the API provided by a compiled
Futhark program.  We have already used this facility to fix an issue
in [literate Futhark](2021-01-18-futharkscript.html) that [prevented
FutharkScript tuples from being passed to Futhark entry
points](https://github.com/diku-dk/futhark/issues/1684), because we
had no idea how to construct the corresponding C object at run-time.
Well, now we do.

## Multiple return values

In Futhark, as in most sensible language, a function returns only a
single value.  "Multiple return values" are implemented by returning a
tuple.  Consider this entry point:

```Futhark
entry f (x: i32) (y: i32) = (x+y, x-y)
```

This returns a value that happens to be a tuple.  It turns into a C
function with this prototype:

```C
int futhark_entry_f(struct futhark_context *ctx,
                    int32_t *out0, int32_t *out1,
                    const int32_t in0, const int32_t in1);
```

The tuple is gone, but instead we have *two* output parameters!  The
Futhark compiler uses the simple rule that if the return value of a
function is a tuple, then that is turned into multiple return values.
This was a convenience we added long ago so that you wouldn't have to
write your own projection functions all the time.  Now that the
compiler has been taught how to automatically generate those
functions, this convenience is more of an ugly inconsistency.
Unfortunately, in order to remain backwards compatible we have to keep
it around.
