---
title: End of a compiler optimisation
author: Troels Henriksen
description: This used to make programs faster (maybe?), but apparently not anymore
---

In this post I will talk about a somewhat unusual optimisation that
the Futhark compiler has been performing for a long time. This post is
written just before that optimisation is removed.

As I've written about [now and
then](2019-02-18-futhark-at-ppopp.html), the Futhark compiler is not a
*parallelising* compiler. It expects you, the honoured programmer, to
use the vocabulary of data parallelism to express your program, but it
will not try to turn your *sequential* loops into parallel code. This
is formalised through [the solemn oath of a parallel cost
model](2022-01-27-cost-models-are-contracts.html).

Futhark thus encourages a style of programming where the programmer
*maximises* the amount of application parallelism. In practice,
however, our computers are not infinitely parallel, and beyond a
certain point, additional parallelism leads to no improvement in
utilisation, but only additional overhead. Essentially all parallelism
involves overhead (even if minuscule in some cases), and parallelism
beyond what is necessary to saturate the machine is detrimental to
performance.

Since the Futhark compiler is supposed to generate code that is fast
in practice, on real computers, it often decides to *sequentialise*
parts of a parallel program, in order to execute it with less
overhead. Also, when we first developed the compiler, we did not have
a parallel code generator at all, and so needed a transformation that
could turn all those `map`s and `reduce`s and whatnot into sequential
loops. We call this *sequentialisation*. Consider sequentialising the
following small expression:

```Futhark
let B = map (\x -> x + 2) A
```

It is immaterial whether we are sequentialising this because we are
using a sequential compiler backend, or because we are deep inside
another parallel computation and this level of parallelism is deemed
unnecessary. In either case, the compiler will turn it into a a loop
that looks like this, assuming `A` is of size `n`:

```Futhark
let B_initial = replicate n 0
let B = loop B = B_initial for i < n do
          let x = A[i]
          let tmp = x + 2
          let B[i] = tmp
          in B
```

This makes use of [in-place updates through uniqueness
types](2022-06-13-uniqueness-types.html) to express the sequentialised
code efficiently. (In fact, the *actual* compiler representation does
not use `replicate`, which requires initialising every element,, but a
special `scratch` construct that creates an array with unspecified
contents, by allocating memory without initialising it.)

The basic sequentialisation rule for `map` is to create an output
array, loop over the input array, recursively sequentialise the lambda
function whatever it might be, and then store the result in the output
array at the proper location. But consider what happens when we *nest*
`map`s:

```futhark
let B = map (\a -> map (\y -> x + 2) a) A
```

Applying sequentialisation we get this, assuming `A` has shape
`[n][m]`:

```Futhark
let B_initial = replicate n (replicate m 0)
let B = loop B = B_initial for i < n do
          let a = A[i]
          let tmp1_initial = replicate m 0
          let tmp1 = loop tmp1 = tmp1_initial for j < m do
                       let x = a[j]
                       let tmp2 = x + 2
                       let tmp1[j] = tmp2
                       in tmp1
          let B[i] = tmp1
          in B
```

There is inefficiency afoot. For each iteration of the outer loop, an
array `tmp1` is constructed, corresponding to the result of the
innermost `map`. This `tmp1` is then copied into `B[i]`. Since
accessing memory is expensive, this redundant copy is not great. It
would be much better if the result of the innermost `map` was
constructed *in-place*, so no copy was necessary:

```Futhark
let B_initial = replicate n (replicate m 0)
let B = loop B = B_initial for i < n do
          let a = A[i]
          let B = loop B_inner = B for j < m do
                    let x = a[j]
                    let tmp2 = x + 2
                    let B_inner[i,j] = tmp2
                    in B_inner
          in B
```

For reasons now lost to time, we decided to address this not by
changing sequentialisation to more directly deal with nesting, but by
introducing a more general optimisation, called *in-place lowering*
because it pushes in-place updates down into loop nests. I suppose we
thought user-written code might also benefit from this. The idea was
to recognise code of the form

```Futhark
let r =
  loop r1 = r0 = for i < n do
    let a = r1[i]
    let r1[i] = ... a ...
    in r1
...
let y[k] = r in
...
```

and turn it into

```Futhark
let x0 = y with [k] <- r0
let y =
  loop x = x0 = for i < n do
    let a = a[k,i]
    let x[k,i] = ... a ...
    in x
...
let y = x0
```

For the above transformation to be valid, a long list of conditions
must be fulfilled. For example, `r` must not be consumed after the
original in-place update (since now that would involve consuming
`y[k]`). Another is that the array `y` (or its aliases) may not be
used inside the body of the loop, as that could violate data
dependencies. [There are
more](https://github.com/diku-dk/futhark/blob/6bf0e8a342fc0fd7b34293201fcc2fa58c595946/src/Futhark/Optimise/InPlaceLowering.hs#L36-L57),
and they are quite fiddly. And to our mild embarrassment, we never
actually managed to check *all* of them. Nevertheless, this
optimisation existed since early 2015, and managed to speed up at
least some programs, some of the time.

During our initial work on the in-place forwarding optimisation, we
realised it was a somewhat limited implementation of a general idea:
avoiding copies by constructing values *in-place*. A mere seven years
later, [Philip Munksgaard](https://munksgaard.me/) finished his PhD on
(among other things) [array short
circuiting](https://futhark-lang.org/blog/2022-11-03-short-circuiting.html),
which is exactly a general implementation of that idea. And only a
year after Philip finished his work, I remembered that short
circuiting in principle makes in-place lowering redundant, [and
decided to remove it](https://github.com/diku-dk/futhark/pull/2055).
Both in-place lowering and array short circuiting are very complicated
program transformations, and I am unsure whether short circuiting
actually handles all the (somewhat ad hoc) cases detected by in-place
lowering. When comparing the performance obtained when disabling the
latter (which is what actually matters), it seems there is no real
impact. And so, after over eight years of service, an optimisation I
added in the first year [of my PhD
studies](https://futhark-lang.org/blog/2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html)
is finally gone.

Good riddance; I really disliked having to fix bugs in that pass.

I still want to improve our sequentialisation transformation some day,
though.
