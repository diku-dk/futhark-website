-- # Exclusive scan
--
-- There are two common ways of specifying [scans](scan-reduce.html): inclusive
-- and exclusive. In an inclusive scan, element *i* of the output includes
-- element *i* of the input, while for an exclusive scan it only goes up to
-- *i-1*. The `scan` functon in Futhark is an inclusive scan, but an exclusive
-- scan is provided as `exscan`. It is defined as follows:

def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : *[n]a =
  scatter (replicate n ne)
          (map (+ 1) (0..1..<n))
          (scan op ne as)

-- It is sometimes useful to have both the inclusive and exclusive scan
-- simultaneously, for example when the two are consumed together in a
-- subsequent `scatter`. The following function computes both in a single pass,
-- returning an array of pairs where the first component is the inclusive scan
-- and the second is the exclusive scan:

def incexscan [n] 't (op: t -> t -> t) (ne: t) (xs: [n]t) : [n](t, t) =
  map (\x -> (x, ne)) xs
  |> scan (\(a1, _) (a2, b2) -> (a1 `op` a2, a1 `op` b2)) (ne, ne)

-- This avoids the `scatter` at the cost of a more complex operator passed to
-- `scan`, and it allows the compiler to fuse the scan with a subsequent
-- consumer of either component.

-- ## See also
--
-- [Exclusive prefix sum](exclusive-prefix-sum.html), [Evaluating
-- polynomials](polynomials.html).
