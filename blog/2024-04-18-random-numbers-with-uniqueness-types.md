---
title: Avoiding RNG bugs through uniqueness types
description: A programmer complained about a source of bugs, and this is how I think it could be fixed.
---

While visiting [Chalmers](https://www.chalmers.se/en/) to give a guest
lecture in their course on Parallel Functional Programming, I met the
student [Samuel Kyletoft](https://samuel.kyletoft.se/), who had
implemented a ray tracer in Futhark. He mentioned that he'd had a
bunch of bugs related to random number generation (RNG), which is
admittedly a bit awkward in Futhark. The main challenge is that you
manually need to maintain the RNG *state*, since Futhark does not
allow side effects.

To illustrate the problem, let us define a small set of functions for
generating random numbers. First, we define a type for storing the RNG
state:

```Futhark
type rng = u32
```

To initialise the state from a seed, we do a few rounds of a hash
function I [found on Stack
Overflow](https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key/12996028#12996028):

```Futhark
def mk_rng (seed: i32) : rng =
  let x = u32.i32 seed
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x)
  in x
```

For the random number generation itself, we will just do a basic
[linear congruential
generator](https://en.wikipedia.org/wiki/Linear_congruential_generator):

```Futhark
def rand (l: i32) (x: rng) : (rng, i32) =
  let a = 48271
  let m = 2147483647
  let rng' = (a * x) % m
  in (rng', i32.u32 (rng' % u32.i32 l))
```

Note now the `rand` function returns both a new state, as well as the
randomly generated number in the range 0 to `l`. We can use `rand` like
this:

```Futhark
def use (seed: i32) =
  let rng = mk_rng seed
  let (rng', x) = rand 6 rng
  let (rng'', y) = rand 6 rng'
  in x + y
```

This works fine. However, it is easy to use an old RNG state by
accident:

```Futhark
def use (seed: i32) =
  let rng = mk_rng seed
  let (rng', x) = rand 6 rng
  let (rng'', y) = rand 6 rng
  in x + y
```

Note how I typed `rng` instead of `rng'` in the last call to `rand`.
In this case the compiler will complain about `rng'` being unused, but
it's not hard to imagine a larger program where `rng'` is indeed used
for something else later. Especially when refactoring, it is easy to
accidentally reuse the same RNG state twice, which will lead to random
numbers being correlated. For a ray tracer, this can result in fun
visual artefacts, but for other programs it may just result in a
number being wrong, which is both boring and tedious to debug.

In an imperative language, generating a random number mutates the
state, so it cannot be reused. In languages such as Haskell, you can
use a state monad to simulate the same thing. In Futhark, it turns out
you can imitate a form of *affine types* using Futhark's [slightly
obscure support for uniqueness
types](2022-06-13-uniqueness-types.html). Affine types allow you to
express that a value can be used *at most once*. A related notion is
*linear types*, where a value must be used *exactly once*, which is
why linear types are useful for resource management, as then the last
use must be a cleanup function. By constructing an RNG library such
that number generation *consumes* a state and *produces* a new one, we
can ensure that each state is used at most once.

Although uniqueness types are really designed for dealing with arrays,
they can also be used for abstract types via [the module
system](https://futhark-lang.org/blog/2017-01-25-futhark-module-system.html).
So first we define a module that describes the RNG interface:

```Futhark
module type rand = {
  type rng
  val mk_rng : i32 -> rng
  val rand : i32 -> *rng -> (rng, i32)
}
```

Note the asterisks on the `rand` parameter type - this denotes a
consuming parameter, meaning the `rng` we pass in may not be used
again.

We implement the module using the same code as above:

```Futhark
module rand : rand = {
  type rng = u32

  def mk_rng (seed: i32) : rng =
    let x = u32.i32 seed
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x)
    in x

  def rand (l: i32) (x: rng) : (rng, i32) =
    let a = 48271u32
    let m = 2147483647u32
    let rng' = (a * x) % m
    in (rng', i32.u32 (rng' % u32.i32 l))
}
```

We can only access the functions through the types defined in the
module type, which means `rand` will consume its `rng` argument,
despite the actual function not doing anything special. Code like this
will now work:

```Futhark
def use (seed: i32) =
  let rng = rand.mk_rng seed
  let (rng', x) = rand.rand 6 rng
  let (rng'', y) = rand.rand 6 rng'
  in x + y
```

But if we try to reuse an RNG state, the type checker will tell us:

```Futhark
def abuse (seed: i32) =
  let rng = rand.mk_rng seed
  let (rng', x) = rand.rand 6 rng
  let (rng'', y) = rand.rand 6 rng
  in x + y
```

```
Error: Using variable "rng", but this was consumed at 3:31-34.
```

If we *want* to duplicate an RNG state, we can still do so, as the
`copy` prelude function can copy anything you can put in an array.
This is explicit, and so unlikely to lead to unintended behaviour.

The most widely used Futhark library for random numbers is
[cpprandom](https://github.com/diku-dk/cpprandom), which does not use
this approach, but I'm wondering whether it would be better if it did.
However, in practice, bugs like this are not too difficult to avoid,
if we simply use shadowing to make the old RNG states inaccessible:

```Futhark
def use (seed: i32) =
  let rng = mk_rng seed
  let (rng, x) = rand 6 rng
  let (rng, y) = rand 6 rng
  in x + y
```
