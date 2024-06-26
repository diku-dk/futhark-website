-- # Exclusive scans
--
-- There are two common ways of specifying [scans](scan-reduce.html):
-- inclusive and exclusive. In an inclusive scan, element *i* of the
-- output includes element *i* of the input, while for an exclusive
-- scan it only goes up to *i-1*. The `scan` functon in Futhark is an
-- inclusive scan, but it is easy to define an exclusive one:

def exscan f ne xs =
  map2 (\i x -> if i == 0 then ne else x)
       (indices xs)
       (rotate (-1) (scan f ne xs))

-- ## See also
--
-- [Evaluating polynomials](polynomials.html).
