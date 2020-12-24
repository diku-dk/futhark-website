-- # Pairwise L₁ distances
--
-- First we define a function for computing the [L₁
-- distance](https://en.wikipedia.org/wiki/Taxicab_geometry) between
-- two vectors:

let L1 [n] (xs: [n]f64) (ys: [n]f64) : f64 =
  map2 (-) xs ys |> map f64.abs |> f64.sum

-- We use the [forward pipe operator](piping.html) in the definition
-- above.
--
-- To compute the pairwise L₁ distances between all rows in a matrix,
-- we `map` over the rows twice, then compute the distances between
-- two rows:

let pairwise_L1 [n][m] (xss: [n][m]f64) : [n][n]f64 =
  map (\a -> map (\b -> L1 a b) xss) xss

-- This pattern is very similar to matrix multiplication.
--
-- ## See also
--
-- [Matrix multiplication](matrix-multiplication.html).
