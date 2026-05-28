-- # Evaluating polynomials with Horner's Method
--
-- A polynomial of the *n*th degree comprises *n* coefficients and can
-- be evaluated at a specific value *x* as follows:

def poly as x =
  f64.sum (map2 (*) as (map (x **) (map f64.i64 (indices as))))

-- [Horner's method](https://en.wikipedia.org/wiki/Horner%27s_method)
-- is a well-known technique for evaluating polynomials using fewer
-- multiplications (in particular, avoiding the use of the power
-- operation). While normally expressed sequentially, it can also be
-- implemented in Futhark using an [exclusive
-- scan](exclusive-scan.html):

def horner as x =
  f64.sum (map2 (*) as (exscan (*) 1 (map (const x) as)))

-- In most cases, the additional overhead of manifesting the result of the
-- `scan` exceeds the savings from not evaluating `**`, so `poly` is sometimes
-- faster than `horner`.
--
-- However, Oleg Kiselyov presents in the article [Parallel Fun with
-- Monoids](https://okmij.org/ftp/Algorithms/map-monoid-reduce.html) a monoid
-- for computing Horner's method, which can be implemented in Futhark as
-- follows.

def horner_oleg as x : f64 =
  let op (p1, b1) (p2, b2) = (p1 * b2 + p2, b1 * b2)
  let (y, _) = reduce op (0, 1) (map (\a -> (a, x)) (reverse as))
  in y

-- The function `horner_oleg` can be expected to run much faster than both
-- `poly` and `horner`.
