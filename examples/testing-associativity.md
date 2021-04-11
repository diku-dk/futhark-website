---
title: Testing for associativity
---

Recall that for [scans and reductions](scan-reduce.html), the
operator given must be *associative*.  This means
that we can move around the parantheses in an application:

```
(x `f` y) `f` z == x `f` (y `f` z)
```

While it is formally undecidable for any compiler to determine
statically whether an arbitrary function `f` is associative.  it's
not so hard to write a test program that looks for a counterexample
to associativity by brute force.

```futhark
let testassoc 'a [n] (eq: a -> a -> bool)
                     (op: a -> a -> a)
                     (ne: a)
                     (arr: [n]a) =
  let hom xs = reduce op ne xs
  let golden = hom arr
  in (.1) <| loop (ok, i) = (true, 0) while ok && i < n do
    let (bef, aft) = split i arr
    in if hom bef `op` hom aft `eq` golden
       then (ok, i + 1) else (false, i)
```

It works by splitting the input `arr` at every possible point,
reducing the two parts separately, combining these partial results
with `f`, and then comparing them to reducing `arr` as a whole.  If
there is a difference (as determined with `eq`), then we have a
counterexample.

For example, we can use it to show that subtraction is not associative:

```
> testassoc (==) (-) 0 [1,0,0,-1]
1i32
```

This says that reducing `[1]` (the first 1 elements) and `[0,0,-1]`
(the remaining 3 elements) of the input array, then combining them
with `(-)`, differs from reducing the whole test array in one go.
Ergo, something is wrong with the operator.  If the operator is
indeed associative, then `testassoc` returns the size of the array:

```
> testassoc (==) (+) 0 [0,0,0,2,2,2,2]
7i32
```

While capable of emitting false positives, in practice this
technique works well, in particular when combined with large
randomly generated input arrays.
