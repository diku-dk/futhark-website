---
title: Reducing or scanning without a neutral element
---

The `reduce` and `scan` functions expect you to provide a neutral
element, such as `0` for addition or `1` for multiplication.  But
sometimes there may not be an obvious neutral element.  In
mathematics, such a structure is called a
[semigroup](https://en.wikipedia.org/wiki/Semigroup), while those
with neutral elements are called
[monoids](https://en.wikipedia.org/wiki/Monoid).

We can always
turn any monoid into a semigroup, simply by adding a distinct new
value to serve as the neutral element.

```futhark
type with_neutral 't = #neutral | #val t
```

The operator must also be augmented to handle the neutral element:

```futhark
let f_with_neutral 't (f: t -> t -> t)
                      (x: with_neutral t)
                      (y: with_neutral t)
                      : with_neutral t =
  match (x, y)
  case (#val x, #val y) -> #val (f x y)
  case (#neutral, _) -> y
  case (_, #neutral) -> x
```

We can then define a variant of `reduce` that does not require a
neutral element to be provided.  If the input array is empty, it
will return the `#neutral` value.

```futhark
let reduce1 't (f: t -> t -> t) (ts: []t) : with_neutral t =
  reduce (f_with_neutral f) #neutral (map (\t -> #val t) ts)
```

Try it out in the REPL:

```
> reduce1 (+) (iota 100)
#val 4950i32
> reduce1 (+) (iota 0)
#neutral
```

`reduce1` is less efficient than `reduce` due to the baggage of
carrying around `#neutral`, as well as the extra control flow.  It
is always better if a neutral element is more naturally available,
but this technique will do in a pinch.

Sadly, no similar trick exists for turning a non-associative
function associative.
