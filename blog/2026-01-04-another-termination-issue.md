---
title: Another Termination Issue
author: Troels Henriksen
description: This time it is the interpreter that is doing something unexpected.
---

Futhark has a rather aggressively optimising compiler, and one of the things it
will do most eagerly is remove code that it determines is not necessary, and
move code that it things can benefit from being moved. This can result in
unusual situations such as infinite loops becoming finite, or a program with
out-of-bounds accesses or division-by-zero not failing at run-time. A couple of
years ago [I wrote a blog post about the
issue](2023-03-17-on-nontermination.html), largely through the perspective of
the optimising compiler terminating *more* than the interpreter. I later wrote a
post about how [the interpreter serves as the reference
implementation](2025-05-07-implement-your-language-twice.html), but that it is
still fine for the interpreter to differ from the compiler - but *if* the
interpreter produces a value, then the compiler should produce that same value.

Well, the trouble with having a language that other people sometimes want to
use, is that they will end up writing programs that require you to answer
uncomfortable questions. This happened yesterday, as a programmer [reported a
discrepancy between the compiler and
interpreter](https://github.com/diku-dk/futhark/issues/2345). For the purpose of
this post, we can distil the program down into this:

```Futhark
def error 'a : a =
  let xs: [0]a = []
  in xs[1]

entry main (b: bool) = if b then error else true
```

The `error` definition is a bit odd, and merits explanation. It is a polymorphic
*value* (not a function!), that for any type `a` constructs an empty array of
type `[0]a` and then tries to index it, immediately causing an out-of-bounds
error. In this program, the `error` definition is used in one place, where type
inference forces it to be instantiated with type `bool`.

The basic compilation model for Futhark programs is that top level definitions
("constants") are evaluated once at program startup. Since top level definitions
have access to the full Futhark language, they may fail. If this happens, then
program startup is aborted, before any entry points are executed. This is pretty
standard for strict languages. *Polymorphic* top level definitions are not
special in this regard - early on in the compilation process, Futhark performs
[monomorphisation](2021-08-02-value-representation.html), where polymorphic
definitions are duplicated and specialised for each distinct type they are used
with (this is also how languages such as C++ and Rust do it).

The interpreter is a little different. Because it is explicitly designed not to
require any pre-processing of the program beyond type checking, it does not
perform monomorphisation, and when it interprets the definition of `error`, it
does not yet know at which types it will be used. Instead, it effectively
represents `error` as essentially a 0-ary function, and at each use it is
instantiated and evaluated. This only happens for polymorphic values -
monomorphic values (which are far more common) are computed only once.

The result of the interpreters' approach is if a polymorphic value is never
used, *then its definition is never executed*. In the program above, if `main`
is provided the input `false`, then `error` is not used, and the program
terminates successfully with the value `false`. With the compiler however,
`error` (instantiated with `a=bool`) is executed at program startup, which then
immediately fails with an out-of-bounds access. Now it is the *interpreter* that
terminates more than the compiler does.

This unfortunately throws some grit into a rule that I thought was pretty
simple: *if* the interpreter produces a value, *then* the compiler should
produce that same value. Now the interpreter produces a value, but the compiler
does not. Yet I don't really want to change the compiler to handle this case
differently, [for reasons discussed
previously](2023-03-17-on-nontermination.html). It is clear that we are stuck in
[another semantic mess](2025-09-26-the-biggest-semantic-mess.html), although
this one fortunately only affects somewhat contrived programs. I don't quite
know how to handle it.

One solution is to declare that the *semantics* of the language require
immediate evaluation of top level definitions, and that the interpreter is
simply wrong, but that would mean we don't have a true reference implementation,
which I dislike.

Another solution is to change the interpreter such that polymorphic values are
evaluated immediately - either by doing an analysis up front determining which
types they are instantiated with (ugh!), or changing our evaluation strategy for
polymorphic values such that we do not need to know their types before we can
execute their definitions (but this is related to [the biggest semantic mess in
Futhark](2025-09-26-the-biggest-semantic-mess.html), and so is not quite
straightforward).

A third solution is to rephrase the rule that related the behaviour of the
interpreter (i.e., language semantics) and permissible optimisations, but I
don't know how to do that either.

Fixing this is not urgent, as the issue only shows up for contrived programs
that have top level values with failing definitions, but I think the issue
raises interesting questions between semantics and optimisations.
