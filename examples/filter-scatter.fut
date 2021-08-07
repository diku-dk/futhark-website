-- # Scattering the result of a filter
--
-- Suppose we have a list of index/value pairs that we wish to
-- [scatter](gather-and-scatter.html) into a target array, *except*
-- for those values that fail some property.  We might write it
-- like this:

let scatter_some 'a (dest: *[]a) (p: a -> bool) (pairs: [](i64, a)) =
  let (is, vs) = unzip (filter (\(_, v) -> p v) pairs)
  in scatter dest is vs

-- This works:
--
-- ```
-- > scatter_some (replicate 10 0) (>0) [(0,2),(3,4),(5,-1)]
-- [2i32, 0i32, 0i32, 4i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32]
-- ```
--
-- But it is suboptimal, because `filter` is a costly operation.  It
-- is better to exploit the property that `scatter` ignores indexes that
-- are out of bounds, and map the "removed" elements to an invalid
-- index, such as -1:

let scatter_some_better 'a (dest: *[]a) (p: a -> bool) (pairs: [](i64, a)) =
  let (is, vs) = unzip (map (\(i, v) -> if p v then (i,v) else (-1,v)) pairs)
  in scatter dest is vs

-- # See also
--
-- [Reducing the result of a filter](filter-reduce.html), [Reducing or
-- scanning without a neutral element](no-neutral-element.html).
