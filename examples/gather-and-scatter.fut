-- # Gather and scatter
--
-- In vector programming, *gather* is an operation that takes a source
-- array *xs* and an array of indexes *is*, and for each *i* in *is*
-- produces the value *xs[i]*.  Futhark does not have a dedicated
-- `gather` function, but we can use a combination of `map` and array
-- indexing to write our own.

let gather 'a (xs: []a) (is: []i32) =
  map (\i -> xs[i]) is

-- *Scatter* is an operation that writes `(index,value)`-pairs to some
-- *destination array*.  This *is* a primitive in Futhark, `scatter`,
-- which is of type
--
-- ```
-- val scatter 'a : (dest: *[]a) -> (is: []i32) -> (vs: []a) -> *[]a
-- ```
--
-- The `*` before on the destination and return type is a *uniqueness
-- annotation*.  We can ignore it if the destination array we pass to
-- `scatter` is always a fresh array created with `replicate` or
-- `copy`:

let xs = scatter (replicate 7 0) [0, 2, 4, 6] [1, 2, 3, 4]

-- `xs` now has the value `[1i32, 0i32, 2i32, 0i32, 3i32, 0i32,
-- 4i32]`.  Of course, this particular value is better constructed
-- using just `map` and `iota`.  For a real use of `scatter`, see [the
-- radix sort example](radix-sort.html).
--
-- `scatter` ignores out-of-bounds writes.  However, there is a risk
-- of writing multiple values to the same index.  This is only allowed
-- if the same value is being written.  For a related construct that
-- can handle duplicate writes in other ways, see
-- [`reduce_by_index`](histograms.html).

