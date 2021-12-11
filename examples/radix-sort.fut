-- # Radix sort
--
-- [Radix sort](https://en.wikipedia.org/wiki/Radix_sort) is a
-- non-comparative sorting algorithm for sorting things that "behave"
-- like numbers, in that they can be decomposed into digits.  This
-- makes it asymptotically more efficient than comparison-based sorts,
-- and it also permits efficient GPU implementation.
--
-- The radix sort we will show here is very simple, and not as fast as
-- it could be.  The main reason is that it only processes a single
-- digit at a time.  Most high-performance implementations consider
-- multiple bits at once.  Also, we only sort 32-bit unsigned
-- integers.  Most of the work is done by a function
-- `radix_sort_step`, where `radix_sort_step xs b` returns `xs` with
-- the elements sorted with respect to bit `b`.

def radix_sort_step [n] (xs: [n]u32) (b: i32): [n]u32 =

-- To demonstrate how it works, suppose
-- ```
-- -- xs = [2, 0, 2, 4, 2, 1, 5, 9]
-- -- b  = 1
-- ```
--
-- First, for each element, we compute whether bit `b` is set.

  let bits = map (\x -> (i32.u32 (x >> u32.i32 b)) & 1) xs
  -- bits = [1, 0, 1, 0, 1, 0, 0, 0]

-- We will also need the negation (swapping 0s and 1s).

  let bits_neg = map (1-) bits
  -- bits_neg = [0, 1, 0, 1, 0, 1, 1, 1]

-- Compute how many elements do not have bit `b` set.

  let offs = reduce (+) 0 bits_neg
  -- offs = 5

-- For those elements that do *not* have bit `b` set, we use a
-- [prefix sum](scan-reduce.html) (`scan (+) 0`) to compute their
-- *1-indexed* positions in the final result.  The elements that
-- *do* have bit `b` set are set to 0 by the `map`.

  let idxs0 = map2 (*) bits_neg (scan (+) 0 bits_neg)
  -- idxs0 = [0, 1, 0, 2, 0, 3, 4, 5]

-- Similarly, compute the final positions for the elements that *do*
-- have bit `b` set - note that we also offset these by `offs`.

  let idxs1 = map2 (*) bits (map (+offs) (scan (+) 0 bits))
  -- idxs1 = [6, 0, 7, 0, 8, 0, 0, 0]

-- Add `idxs0` and `idxs1` together.  This will give a sensible result
-- because they are never nonzero in the same position.

  let idxs2 = map2 (+) idxs0 idxs1
  -- idxs2 = [6, 1, 7, 2, 8, 3, 4, 5]

-- Our calculations have produced 1-indexed offsets, but Futhark
-- arrays are 0-indexed, so decrement each element.

  let idxs  = map (\x->x-1) idxs2
  -- idxs = [5, 0, 6, 1, 7, 2, 3, 4]

-- Finally, copy `xs` (just to have an array of the right size and
-- type) and [scatter](gather-and-scatter.html) the elements of `xs`
-- with the indexes we computed.  We also convert `idxs` to `i64`, as
-- required by the type of `scatter`.

  let xs' = scatter (copy xs) (map i64.i32 idxs) xs
  -- xs' = [0, 4, 1, 5, 9, 2, 2, 2]
  in xs'

-- A full radix sort is then just sequentially looping through each
-- bit position and apply the step function for each.

def radix_sort [n] (xs: [n]u32): [n]u32 =
  loop xs for i < 32 do radix_sort_step xs i

--  A useful optimisation is to first check the position of the most
-- significant bit in each array element, and cap the number of
-- iterations to that.
