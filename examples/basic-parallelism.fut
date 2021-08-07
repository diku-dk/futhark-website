-- # Basic parallelism
--
-- Futhark is a parallel language, but the Futhark compiler is not a
-- *parallelising* compiler.  What this means is that parallelism in
-- Futhark is explicit, and ultimately boils down to a small
-- collection of primitive functions that the compiler knows how to
-- turn into a parallel code.  You cannot simply write down
-- independent subexpressions and expect them to run in parallel: you
-- must use a parallel function.
--
-- The simplest form of parallelism is `map`, which applies a function
-- to each element of an array, producing a new array.

let xs = map (+2) [1,2,3]

-- Now `xs` has the value `[3,4,5]`.  Of course, this is a trivial
-- example.  In most cases, arrays must have tens of thousands of
-- elements for parallel execution to be worthwhile.  However, for
-- these examples, we will stick to arrays of a size that human minds
-- can comprehend.
--
-- You generally don't need to worry about chaining together multiple
-- `map`s, as the compiler performs [map
-- fusion](https://en.wikipedia.org/wiki/Loop_fission_and_fusion) to
-- combine multiple traversals.

let foo = map (*3) (map (+2) [1,2,3])

let bar = map (\x -> (x + 2) * 3) [1,2,3]

-- The definitions `foo` and `bar` will be exactly the same after
-- optimisations, so feel free to use multiple `map`s with simpler
-- functions if it makes the code more readable - it will have no
-- effect on performance.
--
-- Two arrays can be combined to an array of pairs using `zip`:

let pairs = zip xs foo

-- `pairs` now has the value `[(3, 9), (4, 12), (5, 15)]`.  This can
-- be used to implement a function for adding vectors:

let vecadd xs ys =
  map (\(x,y) -> x + y) (zip xs ys)

-- This pattern is quite common, so there is a `map2` function that
-- allows us to forgo the explicit `zip`:

let vecadd_2 xs ys =
  map2 (\x y -> x + y) xs ys

-- One of the great strengths of Futhark is that parallelism can be
-- nested.  For example, to add two matrices, we simply use two nested
-- `map2`s:

let matadd xss yss =
  map2 (\xs ys -> map2 (\x y -> x + y)
                       xs ys)
       xss yss

-- `map` is the simplest parallel function, yet is surprisingly
-- versatile.  Other common parallel function are
-- [reduce](scan-reduce.html) and [scatter](gather-and-scatter.html).
