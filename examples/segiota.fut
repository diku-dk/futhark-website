-- # Irregular segmented iota
--
-- A segmented `iota` takes as input an array of sizes (the *shape vector*),
-- then produces the concatenated `map` of `iota` on that array.
--
-- The idea is based on the observation that `iota n` can be written as an
-- [exclusive prefix sum](exclusive-prefix-sum.html) over an array comprising
-- `n` copies of 1.
--
-- To perform a segmented `iota, we will need an exclusive prefix sum and a
-- segmented scan.

import "exclusive-prefix-sum"
import "segscan"

-- The idea is to first compute the size `m` of the final array, by summing the
-- shape vector. Then we compute a flag array that is all `false`, except it has
-- a `true` when we begin a new `iota` segment - whose positions are given given
-- by the exclusive prefix sum of the shape vector. Finally do a segmented
-- prefix sum, then subtract 1 from the result. The latter would be unnecessary
-- if we used an *exclusive* segmented scan.

def segiota [k] (ns: [k]i64) : ?[m].[m]i64 =
  let m = i64.sum ns
  let offsets = expresum ns
  let flags = spread m false offsets (replicate k true)
  in segscan (+) 0 flags (replicate m 1)
     |> map (\x -> x - 1)

-- > segiota [0,1,2,3,4]

-- Note how this corresponds to the concatenation `iota 0 ++ iota 1 ++ iota 2 ++ iota 3 ++ iota 4`.
