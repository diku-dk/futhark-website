---
title: Hoisting explosive code out of loops
description: An explanation of how Futhark is willing to hoist dangerous things out of loops, and how it is protected.
---

The Futhark compiler uses the term *hoisting* for moving code out of control
structures. The most common example, and where the term originates, is moving
invariant code out of loops, which is called *loop-invariant code motion*. The
potential advantage is that instead of computing some expression once per loop
iteration, we can compute it once and reuse the value for every loop iteration.
However, if the loop actually runs for *zero* iterations, then hoisting the
expression means it is now run *once* instead of *never* - infinitely slower!
Further, maybe that expression is something dangerous, that is only safe exactly
when the loop runs at least once. For example, consider this Futhark loop:

```futhark
loop x = 0 for i < m do
  let inv = a / n
  in x + inv
```

The binding of `inv` is totally invariant to the loop, yet moving it outside of
the loop is potentially dangerous, as when `n==0` but `m==0` (and hence the loop
never runs), we would still perform a [division by
zero](2026-10-14-defining-division.html). Thus, we need to be careful about how
we move code out of loops and similar control structures. In Futhark I recently
[found a bug](https://github.com/diku-dk/futhark/issues/2612) related to
improper handling of this situation, and I figured I'd write a post not just
about how this comes about, but also a nifty trick that Futhark uses to
*actually* hoist such potentially explosive code without changing the behaviour
of the program. (As always, it would be much easier to write compilers if
programmers were not so particular about what their programs actually do.)

## How Futhark does safety checks

Futhark supports various programming constructs that can fail, and we put [a lot
of effort into detecting failure
robustly](2026-11-14-fixing-failure-handling.html) - somewhat unusual for a
high-performance language. One of the tricks we use is that our intermediate
representation separates the notion of *the thing that can fail* from *the check
that it doesn't*. I discussed the design [in another blog post some time
ago](2023-03-17-on-nontermination.html), but let me quickly restate the idea.

A potentially unsafe operation, such as integer division `a / n`, is preceded by
an *assertion* that actually performs the safety check, and fails in a
controlled way if the given conditional is false:

```futhark
let c = assert (n != 0)
```

The `c` that is produced by `assert` is a "certificate" of type `unit` (meaning
it contains no information), and to ensure that the safety check is actually
performed before the division, we add an artificial data dependency. The actual
IR syntax doesn't matter for this blog post, so let's just write it next to the
name in the `let` binding:

```futhark
let inv<c> = a / n
```

This means that the entire loop now looks like this:

```futhark
loop x = 0 for i < m do
  let c = assert (n != 0)
  let inv<c> = a / n
  in x + inv
```

This is how all safety checks in Futhark are handled, whether out-of-bounds
indexes, dynamic size coercions, or divisions by zero. The only difference is
the form of the boolean expression passed to `assert`.

## Moving the safety check

Note that the check `assert (n != 0)` does not actually depend on anything
variant to the loop, so we could hoist it out:

```futhark
let c = assert (n != 0)
loop x = 0 for i < m do
  let inv<c> = a / n
  in x + inv
```

Of course, this is as problematic as moving the division, as now the assertion
fails when `m <= 0` and `n == 0`, even though in that case no division is
performed. Thus, when hoisting an assertion out of a loop, we *protect* it,
which is Futhark lingo for adjusting its condition such that it fires *only*
when the loop runs for at least one iteration:

```futhark
let c = assert (m <= 0 || n != 0)
loop x = 0 for i < m do
  let inv<c> = a / n
  in x + inv
```

Now this code motion has been rendered safe. In many cases, safety checks have
fewer dependencies than the code they protect (e.g. they may depend only on
sizes of arrays, not specific values), and so they can be hoisted even when the
protected statement cannot.

In this case, the idea of "protecting" hoisted code can also be used to move the
division, by wrapping it in a conditional:

```futhark
let inv = if m > 0 then let c = assert (n != 0)
                        let inv'<c> = a / n
                        in inv'
                   else 42
loop x = 0 for i < m do
  x + inv
```

The `42` in the `else` branch is an arbitrary dummy value. We can pick whatever
we want, as the value is never going to be used.

The idea of protecting statements is useful whenever we hoist code out of
control flow - whether that is a `loop` as above, a conditional, or a parallel
loop such as `map`.

## What we did wrong about parallel loops such as `map`

The [bug in question](https://github.com/diku-dk/futhark/issues/2612) was
actually quite banal - we neglected to protect unsafe expressions hoisted out of
`map`. The minimal case was this program, which implements parallel rotation:

```futhark
def rotate [n] (r: i64) (xs: [n]i32) =
  map (\i -> xs[(i + r) % n]) (iota n)
```

The `(i + r) % n` expression induces a safety check `assert(n!=0)`, which was
hoisted out of the `map`, and failed whenever `n==0`, despite the `map` never
executing its body in that case.

The fix was simple: [just remember
protection](https://github.com/diku-dk/futhark/pull/2613), alongside a small
tweak to refuse to hoist expressions that turned out to be very expensive to
protect, and that would not benefit from hoisting anyway. In fact, the most
interesting thing about this tale is that it took us this long to notice such a
serious bug, but I suspect the reason is that it only appears for programs that
operate on empty arrays, and those tend to be rare in real programs.
