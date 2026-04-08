-- # Exclusive prefix sum
--
-- A prefix sum is a [scan](scan-reduce.html) where the operator is addition. An
-- exclusive prefix sum is likewise an [exclusive scan](exclusive-scan.html)
-- where the operator is addition, but because addition has an inverse, an even
-- more efficient implementation is possible:

def expresum [n] (xs: [n]i64) : *[n]i64 =
  map2 (-) (scan (+) 0 xs) xs

-- > expresum [1,2,3,4]
