---
title: When is it OK to modify the numerical behaviour of loops?
description: A dilemma caused by a recent program interacting with an old optimisation.
---

The Futhark compiler is able to optimise certain simple loops to
closed forms. For example, a loop of the form

```Futhark
loop p = init for i < n do
  p && x
```

where `x` is invariant to the loop, will be optimised to ``init &&
x``. This also works for a handful of other types and operators. In
particular, a summation loop such as

```Futhark
loop p = init for i < n do
  p + x
```

turns into `init + n * x`. This is not done by advanced dataflow
analysis, but [a rather simple simplification
rule](https://github.com/diku-dk/futhark/blob/534577205dd3ecce1bffd81f94c496c839da35d8/src/Futhark/Optimise/Simplify/Rules/ClosedForm.hs)
that is really just fancy pattern matching. These optimisations were
initially created long ago for work on program slicing, where you
remove parts of a program result, and then aggressively perform
simplification and code removal in order to end up with a residual
program that has some desirable property. In our case, we wanted to
produce residual programs [that could give us information about the
sizes of things]((../publications/fhpc14.pdf)) (this was long before
[size types](2019-08-03-towards-size-types.html)).

Such sliced programs don't look much like programs written by humans,
and a lot of the simplification rules you use to shrink them will
rarely apply to real programs. We've kept the simplification rules
intact in the simplifier all along however, which does mean Futhark
has unusually powerful facilities for dead code removal. It also means
we have the above-mentioned rules for detecting closed forms of loops.
They haven't been a problem, but recently a [fellow
researcher](https://ashinkarov.github.io/) wrote a program that
behaved oddly. Simplifying slightly, the program was this:

```
def f n = f32.sum (replicate n (1/f32.i64 n))
```

This program computes the sum of `n` floating-point numbers that are
all of the form `1/n`. Mathematically, this gives *1*. With
floating-point arithmetic, roundoff error means it is unlikely to
produce `1` exactly. Using the closed-form loop optimisation above,
the program will be simplified to `1/n*n`, which will usually be `1`.

At first glance, this may look fine. After all, the optimisation
actually makes the program result closer to the idealised mathematical
result. However, the challenge is that the result now depends on
whether a specific optimisation happens to fire. This is often
considered bad practice, because most optimisers are black-box, and it
can be difficult and frustrating for programmers to figure out what is
going on. Also, Futhark's interpreter, which we use as the reference
semantics of the language, does *not* perform any optimisations, and
so will produce a different result from the compiler. This seems
unfortunate. It seems clear to me that this is not an optimisation we
should do, and so we disabled closed-form simplifications for the case
of floating-point arithmetic.

However, there is a twist: The example with `f32.sum` is not a
sequential loop (where the order of evaluation is completely specified
and we ought not to change it), but rather a *parallel* loop - a
[reduction](../examples/scan-reduce.html). We only guarantee
deterministic reductions when the operator is associative, which
floating-point addition is not. Does this give us leeway to optimise
the program in ways that affect the result? Sadly, I would argue that
it does not. Reductions conceptually work by inserting a provided
operator (such as `+`) in between the elements of the array. The
reason we require associativity is because they do not guarantee an
*order of application*, i.e., how the parentheses are placed. But it
is still the case that the result of the reduction will correspond to
*some* possible order of application - the result will not be plucked
out of thin air entirely. In general, I don't think it is the case
that there is *any* summation order for the program above that
completely eliminates the roundoff error for any `n`.

Although the fix to this issue involved disabling an optimisation, I
don't think there is any impact on real programs. It did once again
remind me of the uncomfortable truth that we don't have any
experienced numerical programmers in the Futhark team - our background
is in things like type theory, programming languages, and
high-performance computing - so for subtle issues like this, it is
difficult for us to know whether we are erring too much on the side of
semantic predictability.
