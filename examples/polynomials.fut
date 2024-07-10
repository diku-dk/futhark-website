-- # Evaluating polynomials
--
-- A polynomial of the *n*th degree comprises *n* coefficients and can
-- be evaluated at a specific value *x* as follows:

def poly as x =
  f64.sum (map2 (*) as (map (x**) (map f64.i64 (indices as))))

-- [Horner's method](https://en.wikipedia.org/wiki/Horner%27s_method)
-- is a well-known technique for evaluating polynomials using fewer
-- multiplications (in particular, avoiding the use of the power
-- operation). While normally expressed sequentially, it can also be
-- implemented in Futhark using an [exclusive
-- scan](exclusive-scan.html):

import "exclusive-scan"

def horner as x =
  f64.sum (map2 (*) as (exscan (*) 1 (map (const x) as)))

-- In most cases, the additional overhead of manifesting the result of
-- the `scan` exceeds the savings from not evaluating `**`, so `poly`
-- is often fastest.
