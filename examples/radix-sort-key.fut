-- # Radix sort by key
--
-- We often wish to sort an array by some property of the elements,
-- rather than the elements themselves.  The [radix
-- sort](radix-sort.html) example can be modified to allow this by
-- parameterising over a function `f` of type `t -> u32` to sort an
-- array of type-`t` elements by the integer produced by `f`.

def radix_sort_step [n] 't (f: t -> u32) (xs: [n]t) (b: i32): [n]t =
  let bits = map (\x -> (i32.u32 (f x >> u32.i32 b)) & 1) xs
  let bits_neg = map (1-) bits
  let offs = reduce (+) 0 bits_neg
  let idxs0 = map2 (*) bits_neg (scan (+) 0 bits_neg)
  let idxs1 = map2 (*) bits (map (+offs) (scan (+) 0 bits))
  let idxs2 = map2 (+) idxs0 idxs1
  let idxs  = map (\x->x-1) idxs2
  let xs' = scatter (copy xs) (map i64.i32 idxs) xs
  in xs'

def radix_sort [n] 't (f: t -> u32) (xs: [n]t): [n]t =
  loop xs for i < 32 do radix_sort_step f xs i

-- The only change compared to the original radix sort is that we use
-- `f` to extract a representative integer.

-- # See also
--
-- [Merge sort](merge-sort.html), [matching parentheses](parens.html).
--
-- The [sorts](https://github.com/diku-dk/sorts) library.
