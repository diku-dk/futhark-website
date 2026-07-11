---
title: Breaking ten years of API compatibility
description: The Futhark C API has been remarkably stable, but it is unfortunately time to break it.
---

Some years ago I wrote a blog post on [design flaws in
Futhark](2019-12-18-design-flaws-in-futhark.html), listing four things we got
wrong. Some years later, I made a [triumphant
post](2021-12-19-past-and-present.html) that the last flaw had been fixed and
Futhark is now perfect. In a recent [release
announcement](2026-02-02-futhark-0.25.35-released.html) I even pondered how long
it had been since we broke compatibility in a way that users would notice. Well,
unfortunately, I forgot one nagging itch that has now become intolerable, and it
is soon time to break compatibility in one of the areas where Futhark has been
almost completely stable since the earliest beginning - [its C
API](2017-09-26-calling-futhark-from-c-and-haskell.html). (And also [its Python
API](2016-04-15-futhark-and-pyopencl.html), for which the same story applies.)

First, some preliminaries to set the stage. Futhark is not a general purpose
programming language - since it is completely pure, with no capability for IO
what-so-ever, you cannot write full applications in it, as you have no way to
interact with the world. While there is some automation for writing test suites,
in practice a Futhark program is compiled to a library, which is then invoked by
code written in general-purpose languages. Most of Futhark's backends generate C
code, and so the way you interact with a compiled Futhark program is via [a C
API](https://futhark.readthedocs.io/en/latest/c-api.html). This API is somewhat
tedious to use, but is designed to be easy to target through automated bridges
that generate idiomatic wrapper code for high level languages, [such as this one
for Standard ML](2023-10-13-smlfut.html), and also for [other
languages](https://futhark-lang.org/docs.html#bridges).

Each Futhark entry point function becomes a C function, and each Futhark
function parameter become a C function parameter. A Futhark function that
accepts an `i32` and a `f64` becomes a C function that accepts a `int32_t` and a
`double` (as well as a "context" parameter for internal state management). The
return type is a little more challenging. It is not so difficult for a function
that returns a single number, like this one:

```Futhark
entry main (x: i32) (y: f64) = x + i32.f64 y
```

The return value is returned through an "out"-parameter, while the C return type
is an error code:

```C
int futhark_entry_main(struct futhark_context *ctx,
                       int32_t *out,
                       const int32_t in0, const double in1);
```

However, what happens if you return (or accept) a Futhark type that does not
have an obvious representation in C? For primitive types, Futhark uses a
representation close to the machine, but compound types such as tuples and
arrays [use a less obvious value
representation](2021-08-02-value-representation.html), where we do not want to
expose the internal representation. Things are even more complicated for arrays
of compound types, where we *really* do not want to expose the representation at
the API level. This problem cropped up early in [Futhark's
history](2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html),
as we of course wanted to write test programs that produced more than one
result. As Futhark is fundamentally a standard lambda calculus language,
functions return only one value, but that value can of course be a tuple, which
is often informally considered returning "multiple values":

```Futhark
entry main (x: i32) (y: f64) = (x, y)
```

This gave rise to a C function of this type:

```C
int futhark_entry_main(struct futhark_context *ctx,
                       int32_t *out0, double *out1,
                       const int32_t in0, const double in1);
```

Look: two out-parameters! We instituted a policy that if a function returns a
tuple, it is automatically unpacked and returned as its individual components.
This policy applied only to return types; if you wrote a function with a tuple
*parameter*, such as this one:

```Futhark
entry main ((x, y): (i32, f64)) = x + i32.f64 y
```

Then you would get this C function:

```C
int futhark_entry_main(struct futhark_context *ctx,
                       int32_t *out0,
                       const struct futhark_opaque_tup2_i32_f64 *in0);
```

That `struct futhark_opaque_tup2_i32_f64` is an opaque struct that has no
defined representation at the C level. You have no way to construct a value, or
inspects its contents, except through the mechanisms provided by the generated C
API. Originally, and for a very long time, the only way to construct this type
(which also used to have a much less readable name - basically a hash of the
representation) was to define a Futhark entry point that made it for you:

```Futhark
entry mk_pair (x: i32) (y: f64) = (x, y)
```

This is why we decided on the automatic unpacking of tuples at the API level -
otherwise there was no way to actually get at the result, unless you wrote your
own boilerplate entry points.

Eventually we [started automatically generating API
functions](https://github.com/diku-dk/futhark/issues/1568) for constructing and
manipulating opaque types that represent tuples or records, since their
semantics are not secret - only the specific implementation. This means that the
automatic unpacking is no longer *necessary*, since you can just extract the
tuple components you care about using the C API. These API functions are nothing
fancy, and for the example above look like this:

```C
// Construction
int futhark_new_opaque_tup2_i32_f64(struct futhark_context *ctx,
                                    struct futhark_opaque_tup2_i32_f64 **out,
                                    const int32_t f_0, const double f_1);

// Projection
int futhark_project_opaque_tup2_i32_f64_0(struct futhark_context *ctx,
                                          int32_t *out,
                                          const struct futhark_opaque_tup2_i32_f64 *obj);
int futhark_project_opaque_tup2_i32_f64_1(struct futhark_context *ctx,
                                          double *out,
                                          const struct futhark_opaque_tup2_i32_f64 *obj);
```

While automatic unpacking is thus no longer necessary, it was not just an
obsolete convenience, but also turned up as an awkward special case in tools.
For example, the ongoing work on [property-based
testing](2026-03-25-property-based-testing.html) encountered the issue that when
you want to write a generator for a type that happens to be a tuple, then the
tooling has to notice that the generator returns multiple values, and
automatically pack it as the tuple that the eventual user expects. Essentially,
the problem is that in the *actual* Futhark language, if you have two functions

```
val f : a -> (c,d)
val g : (c,d) -> e
```

then their composition `g (f x)` is well-typed, but when viewed through the C
API, and derived APIs such as [the server
protocol](2021-01-18-futharkscript.html), then the composition no longer just
works, as `f` returns "multiple values" instead of a tuple. We have a semantic
gap between the C API and the types in the original Futhark program. This is a
real fly in ointment.

Fortunately the solution is simple: [we just stop doing
that](https://github.com/diku-dk/futhark/issues/2403). Unfortunately, the
consequences are somewhat far-reaching. While no Futhark program stops working,
as this difference is invisible at the language level, it will affect any
program that incorporates a Futhark component that has entry points returning
tuples.

The fix is not difficult: just project the necessary tuple components yourself,
through the provided C functions, or remove whatever workarounds you had in
place to re-pack tuples, depending on which side of the
convenience/inconvenience divide your usage fell on. But it *does* require code
changes, and there is probably a long tail of unmaintained projects that will
stop working with newer versions of the Futhark compiler. I am not insensitive
to this problem, and I do regret that we are in this situation. It is quite
possible that we will regret this change, although I hope not.

Looking back, I'm not sure what we could have done to avoid the situation. Very
early on we had a very concrete need (being able to work with functions that
return multiple values), which we solved with a rather benign heuristic/hack. We
later on came up with a more principled solution (automated accessor functions),
but it built on *years* of infrastructural work. There is no technical reason we
could not have done so from the start, but it would have been a major investment
of time during the early prototyping stages of the language. If anything, the
main sin is that we did not remove the hack as soon as we could.

Also, I *did* consider adding a compiler toggle to make the new behaviour
opt-in, but [for the usual
reasons](2018-06-18-designing-a-programming-language-for-the-desert.html) I did
not want to maintain and test a different path in the code generator, document
multiple behaviours, or make users think about such an obscure detail.
