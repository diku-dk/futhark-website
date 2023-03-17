---
title: On Nontermination and Optimisation in Futhark
author: Troels Henriksen
description: How fast can you make an infinite loop go anyway?
---

We claim that Futhark a purely functional language - entirely free of
side effects!  And it really *is* pure, unlike the ostensibly pure
Haskell which somehow still found it useful to add a highly efficient
concurrent IO manager to its runtime system.

The reason Futhark is pure is because it simplifies program
transformation, and program transformation in the form of automatic
compiler optimisation is the whole reason Futhark exists.  In this
post I will talk about how Futhark - as is clearly customary among
purely functional languages - still has to deal with effects, and how
our handling of them has evolved.

## Optimising away effects

*The* core principle underlying compiler optimisation is that
optimisation must not change the result of the program.  If only
programmers did not care so much about the program result, the life of
the compiler writer would be much simpler.  We can make this principle
a bit more precise by saying that when the original program `p(x)`
applied to an input `x` produces some result, then optimised program
applied to the same input (`p_opt(x)`) must produce the same result.
(This can be made even more precise by involving the actual formal
semantics of the language and specifying what "same result" means, but
let's keep it high level here.)

One of the simplest optimisations is removing dead code.  If some
part of the program is never run, then it is clearly safe to remove.
Closely related is code that is run and computes a result, but where
the result is never used:

```Futhark
let x' = f x
let y' = f y
in y'
```

Since `x'` is not used in the result, we can remove it without
changing the program result.  It is only that easy because Futhark is
pure.  If `f` had any side effects, removing it would only be valid if
those effects had no influence on the program result.  In general,
determining this can require quite complicated inter-procedural
analysis.  The purity of Futhark makes it much easier to identify when
code can be removed or re-ordered - it is safe whenever explicit data
dependencies are preserved.

## Becoming correct

Of course, Futhark is only pure if you ignore one particularly thorny
effect: *nontermination*.  This category of effects denotes not just
the obvious one of infinite loops, but also things such as integer
division by zero and out-of-bounds array accesses.  Futhark does not
allow the handling of such errors from within the program itself
([although they can be detected from the
outside](https://futhark.readthedocs.io/en/latest/c-api.html#error-codes)),
and are thus considered outside the language semantics.  Consider this
expression:

```Futhark
let x = a/b
let y = c/d
in y
```

Since `x` is unused we can remove it as dead.  However, what if there
is some input where `b=0, d!=0`?  Then the non-optimised program will
perform a catastrophic division by zero, while the optimised program
will terminate successfully.  This is a deviation from our core
principle that optimisation should not change the program result.  Yet
is awfully inconvenient for the compiler to preserve all
nontermination, since it can lurk behind any function call.  Also, if
we care to preserve nontermination, we may also desire to preserve the
*kind* of nontermination - an infinite loop is distinct from an
out-of-bounds access - and then we are suddenly very constrained in
how we reorder and remove code!  We are close to losing the
ease-of-transformation that makes Futhark attractive in the first
place.


When convenience clashes with principles, the ~~easiest~~ best
solution is to revise one's principles.  Thus we redefine our notion of
valid optimisation to state that *if* `p(x)` terminates with some
value `v`, then `p_opt(x)` must terminate with that same value `v`.
If `p(x)` does not terminate, then `p_opt(x)` may or may not terminate
with a value.  In truth, I would prefer a slightly stronger statement
that constraints the values that `p_opt(x)` may produce - after all,
it is not allowed to do *anything*.  We do not wish to join the
wild ride of C-style undefined behaviour.  So far it is not yet clear
to how to succinctly and precisely constrain the behaviour of
`p_opt(x)`.  Clearly, execution should still be memory-safe and all
that, and the value produced should have the right type.

## Becoming useful

But other issues remain.  Let us consider a slightly more complicated
optimisation.  The function `iota n` produces an integer array
`[0,1,...,n-1]`.  This means that if

```Futhark
let xs = iota n
```

then

```Futhark
let x = xs[i]
```

can be optimised to

```Futhark
let x = i
```

But hold on!  That array index might fail if `i`, which might be user
input, is out of bounds.  Our optimisation removed an instance of
nontermination.  Strictly speaking, this is in accordance with our
revised principles.  Such an optimisation is *technically* correct,
which is of course the best kind of correct.  But is it *useful*?
Some forms of nontermination serve an important purpose as input
validation, and programmers may be dismayed to find that their
programs produce (well-typed!) garbage for invalid input.  These
programmers may not even be consoled by a reminder that the compiler
is *technically* correct.

Basically, we wish to preserve the ability to easily rewrite the
program, while preserving safety checks.  To accomplish this, [the Futhark
intermediate
representation](https://hackage.haskell.org/package/futhark-0.24.1/docs/Futhark-IR-Syntax.html)
makes safety checks explicit with an `assert` expression:

```Futhark
let c = assert (0 <= i && i < n)
```

If `assert` is passed a false value, it will halt execution with an
error.  Otherwise it returns a "certificate"; a dummy value of type
`unit` that has no run-time representation.  To prevent `assert` from
being removed as dead code, we add a special data dependency to the
expression guarded by the assertion:

```Futhark
let x = <c> xs[i]
```

That `<c>` notation is a "certificate list" that forces whatever
expression produces `c` (the `assert`) to be computed before the array
index expression.  Essentially we have taken a [control
dependency](https://en.wikipedia.org/wiki/Dependence_analysis#Control_dependencies)
and turned it into an explicit data dependency.  This means the
compiler can continue to reason solely in terms of data dependencies.

Optimisations now merely have to propagate these certificate
dependencies, such as for the `iota`-indexing example:

```Futhark
let x = <c> i
```

If the program contains multiple sources of nontermination that do not
directly depend on each other, it is still not guaranteed which of
them will take precedence at runtime.  That is one sacrifice we must
make in order to make code motion reasonably easy.

The main downside is that every optimisation must be careful to do
this properly.  However, the principle is not terribly complicated:
simply take the union of the certificates of the expressions whose
information is being considered while performing the optimisation. And
if you miss one, well, it's still *technically* correct.

Another advantage of this scheme is that now safety checks can be
optimised and moved independently of the expressions they protect -
for example to hoist them out of loops.  That was in fact the original
motivation for this design, which came [very early on in the
development of
Futhark](2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html#starting-my-phd),
before we figured out [how to perform bounds checking on a
GPU](2020-07-13-bounds-checking.html).
