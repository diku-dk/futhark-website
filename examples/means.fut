-- # Means
--
-- The [arithmetic
-- mean](https://en.wikipedia.org/wiki/Arithmetic_mean) of an array of
-- `f64` numbers can be computed as follows:

def mean [n] (vs: [n]f64) =
  f64.sum vs / f64.i64 n

-- > mean [1,2,3,4]

-- The geometric mean is likewise simple:

def gmean [n] (xs: [n]f64) =
  f64.product xs ** (1/f64.i64 n)

-- > gmean [1,2,3,4]

-- # See also
--
-- [Variance](variance.html).
--
-- The [statistics](https://github.com/diku-dk/statistics) package.
