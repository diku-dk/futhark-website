---
title: On prefix operators
description: They're (mostly) gone now.
---

This post will contain more [navel-gazing
pontificating](https://futhark-lang.org/blog/2021-08-05-half-precision-floats.html)
of [minor implementation
details](https://futhark-lang.org/blog/2021-08-02-value-representation.html) -
apparently that is the [ink in my
pen](https://sigkill.dk/writings/fountain_pens.html) these days.  The
topic is prefix operators, which allow us to write `-x`.  Even in
math, these lead to exciting debates about the precise meaning of
expressions such as `-2²` - should it be interpreted as `(-2)²` or
`-(2²)`?  In programming we could of course avoid such confusion if we
were smart enough to just use Lisp, but we're not.  So quick quiz: in
C, should `- 2+2` be interpreted as `-(2+2)` or `(-2)+2` Turns out it's
the latter.  Generally, the rule is that prefix operators bind more
tightly than binary infix operators. (Except `->`, if you consider
that to be an infix operator.)

Alright, so we might not be smart enough to use Lisp, but what if we
are smart enough to use a lambda calculus-derived functional language,
where function application is written with juxtaposition?  When `f x`
is an application of `f` to `x`, then surely there is no need to
distinguish prefix operators from ordinary functions?  Unfortunately,
we are not so lucky.  Due to backwards compatibility with centuries of
mathematical convention, we use the symbol `-` to mean both negation
and subtraction.  This creates ambiguities.  Should `f -x` be viewed
as `(f) - (x)` or `f (-x)`?  Unless you have a very fancy type system,
only one of these is likely to be type-correct (although even with a
fancy type system, only one of these is going to be *what you want*).
So could we use type information to resolve the ambiguity?  Perhaps,
but I would like to be able to parse a program in the absence of type
information.  Mixing the phases of a compiler tends to confuse both
humans and machines.  Standard ML was brave enough to simply use a
different symbol for negation (`~`), but I am too much of a coward to
make the same choice for Futhark.

But the ambiguities are not the only problem.  It is also very
convenient (and common) to write `-sin(x)` and have it be parsed as
`-(sin(x))` rather than `(-sin)(x)`.  This means prefix operators are
syntactically not just functions - they have less binding power.
Could we disambiguate with whitespace, such that `-sin(x)` means
`(-sin)(x)` and `- sin(x)` means `-(sin(x))`?  It's technically
feasible, and whitespace-based disambiguation [is used elsewhere in
Futhark](https://futhark-lang.org/blog/2016-12-09-two-syntax-design-problems.html#array-indexing),
but requiring a space after negation goes against common convention.
Since our desire to support conventional notation is the reason we are
in this mess in the first place, a solution that goes against
convention is not a solution at all.  Hence, we must support prefix
operators with special productions and terminals in our grammar.

## User-defined operators

Futhark supports user-defined infix operators:

```Futhark
let (x,y) ++ (a,b) = (x+a,y+b)
```

This is done by lexically distinguishing alphanumeric *identifiers*
and symbolic *operators*, just like in Haskell, OCaml, and F#.  What
about user-defined *prefix* operators?  In principle we can define
some rules that lexically distinguish prefix operators from infix
operators.  F# has a [rather elaborate
scheme](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading#prefix-and-infix-operators),
which defines repeated sequences of `!` and `~` to be prefix
operators, as well as operators with names prefixed by `~` when
defining (but not using!) them.  Futhark used to have a slightly
simplified form of this, that only supported the repeated `!`s and
`~`s.  I don't think it was ever used.  Why would you really want a
function called `!!!!`?

Recently I decided, motivated [by a bug in prefix operator
parsing](https://github.com/diku-dk/futhark/issues/1419), that this
stuff is in the category of language details that nobody enjoys
learning, and nobody wants to remember.  The `~` operator (bitwise
negation) had already been unified with `!` some time ago, and I
decided that `-` and `!`  might as well be the only prefix operators
that we support.  This lets us handle them specially in the parser and
AST, rather than having a more complicated generic scheme.

Since basically no Futhark code found in the wild defines prefix
operators, I thought this would be an almost invisible change.
However, I had forgotten that the builtin prelude itself defines
functions called `!`  in the modules for numeric types:

```Futhark
type t
val ! : t -> t
```

These have been renamed to `not`, to match the already existing `neg`.
I do worry a little about confusion between numeric negation (`neg`)
and logical/bitwise negation (`not`), especially as they have the same
type.

While the ability to definite new prefix operators has been removed
from the language, what about overloading the remaining ones?  Well,
for now, Futhark does not allow *any* user-defined overloading at all,
but it is certainly something we hope to add in the future.  In
Haskell, the `-` prefix operator is translated to a call to the
`negate` method in the `Num` type class, which is then resolved using
the normal instance resolution mechanisms.  We would presumably do it
the same way in Futhark.  While it is a bit magical that `-` becomes
`negate`, such are the compromises needed when you are neither smart
enough to just use Lisp, nor brave enough to just use Standard ML.
