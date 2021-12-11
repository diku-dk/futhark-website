-- # Counting elements that satisfy property
--
-- Suppose we wish to count the number of positive integers in
-- some array.  We might write the following:

def number_pos (xs: []i32) =
  length (filter (>0) xs)

-- This is inefficient, because the program will manifest the result
-- of the filter in memory.  A better solution is to rewrite this as a
-- `map`-`reduce` composition, where we turn positive integers into
-- `1` and others into `0`:

def number_pos_better (xs: []i32) =
  i64.sum (map (\x -> if x > 0 then 1 else 0) xs)

-- This is much more efficient, because the `map` can be fused into
-- the `reduce` that is used to implement `i64.sum`.
--
-- We can write it more concisely by using a built-in function to
-- convert booleans to integers:

def number_pos_best (xs: []i32) =
  i64.sum (map ((>0) >-> i64.bool) xs)

-- And of course, we can factor all of this into a handy function:

def count p xs =
  i64.sum (map (p >-> i64.bool) xs)

-- And now we can define our original function as follows:

def count_number_pos (xs: []i32) =
  count (>0) xs

-- # See also
--
-- [Reducing the result of a filter](filter-reduce.html), [Scattering
-- the result of a filter](filter-scatter.html).
