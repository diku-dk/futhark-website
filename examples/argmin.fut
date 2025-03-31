-- # Index of smallest element (argmin)
--
-- This function finds the index and value of the smallest element of
-- an array of `f64` values. If the input array is empty, it returns
-- an index of zero.

def argmin [n] (xs: []f64) : (f64, i64) =
  reduce_comm (\(vx, ix) (vy, iy) ->
                 if vx < vy || (vx == vy && ix < iy)
                 then (vx, ix)
                 else (vy, iy))
              (f64.highest, n)
              (zip xs (iota n))

-- > argmin [1, 5, -1, 3]

-- The operator is carefully written to be commutative, specifically
-- by picking the element with the lowest index in case of a tie,
-- which allows slightly more efficient code generation.

-- > argmin [1, 0, 1, 0]
