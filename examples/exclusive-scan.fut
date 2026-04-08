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

-- ## See also
--
-- [Exclusive prefix sum](exclusive-prefix-sum.html), [Evaluating
-- polynomials](polynomials.html).
