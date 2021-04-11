---
title: Reducing the result of a filter
---

Suppose we wish to sum all the positive values of some array.  The
obvious way to write it is as follows:

```futhark
let sum_pos (xs: []i32) = reduce (+) 0 (filter (>0) xs)
```

This gives the right result, but it is not optimal.  Since `filter`
is a reasonably expensive operation, it is better to implement this
pattern by `map`ing the "removed" elements to the [neutral
element](scan-reduce.html):

```futhark
let sum_pos_better (xs: []i32) =
  reduce (+) 0 (map (\x -> if x > 0 then x else 0) xs)
```

These `map`-`reduce` compositions are one of the most efficient
parallel programming patterns - quite parallel, and with low
execution overhead.  If we wish, we can factor this
filter-then-reduce strategy into a separate higher-order function:

```futhark
let reduce_some 'a (op: a -> a -> a) (ne: a) (p: a -> bool) (xs: []a) : a =
  reduce op ne (map (\x -> if p x then x else ne) xs)
```

Then we can define our function as simply:

```futhark
let sum_pos_best (xs: []i32) =
  reduce_some (+) 0 (>0) xs
```

# See also

[Reducing or scanning without a neutral
element](no-neutral-element.html), [Scattering the result of a
filter](filter-scatter.html).
