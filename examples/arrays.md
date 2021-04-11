---
title: Arrays
---

Futhark arrays are written as values enclosed in square brackets.

```futhark
let arr = [1,2,3]
```

The type of `arr` is `[]i32` (or `[3]i32` to be pedantic).  The
element type of an array can be another array:

```futhark
let marr = [[1,2,3], [4,5,6]]
```

The type of `marr` is `[][]i32`.  Arrays must be *regular*, meaning
that all elements must have the same shape.  An array such as
`[[1,2], [3]` is illegal.

Arrays can be indexed from zero:

```futhark
let first = arr[0]
```

And sliced:

```futhark
let tail = arr[1:]
```

We can even slice or index only some dimensions:

```futhark
let innermost = marr[:,1:2]
```

This produces `[[2], [5]]`.  Slices are inclusive in the starting
position and exclusive in the ending position [as Dijkstra wills
it](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html).

Strides are also supported.  Generally, The expression `a[i:j:s]`
returns a slice of the array `a` from index `i` (inclusive) to `j`
(exclusive) with a stride of `s`.  If the stride is positive, then
``i <= j`` must hold, and if the stride is negative, then ``j <=
i`` must hold.  For example, we can reverse an array like this:

```futhark
let arr_reversed = arr[::-1]
```

Some syntactic sugar is provided for concisely specifying arrays of
intervals of integers. The expression `x...y` produces an array
of the integers from `x` to `y`, both inclusive. The upper
bound can be made exclusive by writing `x..<y`. For example:

```futhark
let arr_range = 1...3
```

It is usually necessary to enclose a range expression in
parentheses, because they bind very loosely.  A stride can be
provided by writing `x..y...z`, with the interpretation "first
`x`, then `y`, up to `z`". For example:

```futhark
let strided = 1..3...7
```

Now `strided` contains `[1, 3, 5, 7]`.

# See also

Reference manual:
[slicing](https://futhark.readthedocs.io/en/stable/language-reference.html#a-i-j-s).
