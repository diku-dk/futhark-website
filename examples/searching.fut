-- # Searching
--
-- Sometimes you need to find the first element of an array that
-- satisfies some property.  This can be done with a
-- [`map`](basic-parallelism.html)-[`reduce`](scan-reduce.html)
-- composition, where the `map` tags each element with its index and
-- whether it is the element we are looking for, and then a reduction
-- operator `op` that picks the match with the lowest index one.
-- While I know that `op` is
-- [commutative](https://en.wikipedia.org/wiki/Commutative_property),
-- the compiler cannot figure this out on its own, so I use
-- `reduce_comm` to tell it explicitly.  This permits slightly more
-- efficient code generation for the reduction.

let find_index 'a [n] (p: a -> bool) (as: [n]a): i32 =
  let op (x, i) (y, j) =
    if x && y then if i < j
                   then (x, i)
                   else (y, j)
    else if y then (y, j)
    else (x, i)
  in (reduce_comm op (false, -1) (zip (map p as) (iota n))).1

-- Note that if no element is acceptable, we return the index -1.  An
-- alternative could be to use a [sum type](sum-types.html) to return
-- the actual element we found:

type found 'a = #found a | #not_found

let find_elem 'a [n] (p: a -> bool) (as: [n]a) : found a =
  let tag x = if p x then #found x else #not_found
  let op x y =
    match (x,y)
    case (#found _, _) -> x
    case (_, #found _) -> y
    case _             -> x
  in reduce_comm op #not_found (map tag as)

-- Both of these solutions share a potential problem: they will always
-- look at the entire array, even if the element we are looking for is
-- near the beginning.  When searching in parallel, some redundant (or
-- "speculative") work is necessary.  But can we constrain it?  We can
-- of course always just write a [sequential](loops.html) search:

let find_elem_seq 'a [n] (p: a -> bool) (as: [n]a) : found a =
  (loop (_res, i) = (#not_found, 0)
   while i < n do
     if p as[i]
     then (#found as[i], n)
     else (#not_found, i+1)).0

-- No parallelism here.  We can combine the sequential and the
-- parallel approaches by writing a function that sequentially
-- iterates through large chunks of the input, searches each chunk in
-- parallel, and stops at the first match.

let find_elem_chunked 'a [n] (k: i32) (p: a -> bool) (as: [n]a) : found a =
  (loop (_res, chunk_offset) = (#not_found, 0)
   while chunk_offset < n do
     match find_elem p as[chunk_offset: i32.min n (chunk_offset+k)]
     case #found x -> (#found x, n)
     case #not_found -> (#not_found, chunk_offset+k)).0

-- In most cases, the original `find_index` and `find_elem` will be
-- fast enough, as these simple `map`-`reduce` compositions are quire
-- efficient.  While `find_elem_chunked` can in some cases be faster,
-- we now have the chunk size, `k`, as a tunable parameter that we
-- have to worry about, and the compiler cannot help us figure out the
-- best value.
