-- # Computing histograms
--
-- Mathematically, a
-- [histogram](https://en.wikipedia.org/wiki/Histogram#Mathematical_definition)
-- is a discretised representation of a probability distribution.  A
-- histogram computation takes as input a collection of elements, maps
-- each to one of *k* *bins*, and counts the number of elements that
-- fall into each bin (discarding invalid bins).  In Futhark,
-- histogram-like computations can be done with `reduce_by_index`:

def histogram [n] (k: i64) (is: [n]i64): [k]i32 =
  let bins = replicate k 0
  in reduce_by_index bins (+) 0 is (replicate n 1)

-- For example, `histogram 3 [0, 1, 3, 2, 1, 0, 0, 1]` produces
-- `[3, 3, 1]`.  Note that out-of-bounds bins (the `3`) are
-- ignored.
--
-- `reduce_by_index` is a surprisingly flexible function.  In
-- imperative pseudocode, we can describe the behaviour of
-- `reduce_by_index dest f ne is as` as:
--
--     for j < length is:
--       i = is[j]
--       a = as[j]
--       if i >= 0 && i < length dest:
--         dest[i] = f(dest[i], a)
--
-- The `f` function must be associative and have `ne` as its neutral
-- element ([like with scans and reductions](scan-reduce.html)).
-- Furthermore, it must also be *commutative*, which simply means that
-- `f x y == f y x`.
