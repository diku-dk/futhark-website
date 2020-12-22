-- # Size parameters
--
-- Size annotations are used for expressing constraints on the size of
-- array-typed function parameters, and guarantees on the result.  For
-- example, we can promise that a function always returns a
-- single-element array:

let singleton (x: i32): [1]i32 =
  [x]

-- For expressing constraints among the sizes of the parameters,
-- Futhark provides *size parameters*.  Mathematically, the [dot
-- product](https://en.wikipedia.org/wiki/Dot_product) is only defined
-- for vectors of the same length.  Size parameters let us express
-- this in Futhark.

let dotprod [n] (xs: [n]i32) (ys: [n]i32): i32 =
  reduce (+) 0 (map2 (*) xs ys)

-- As [type parameters](parametric-polymorphism.html), size parameters
-- are not passed explicitly when calling the function, but inferred
-- from the value arguments.

let res = dotprod [1,2,3] [4,5,6]

-- ## See also
--
-- [Matrix multiplication](matrix-multiplication.html), [Triangular arrays](triangular.html).
