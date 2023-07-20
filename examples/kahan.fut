-- # Kahan summation
--
-- Summation of floating-point numbers is vulnerable to roundoff
-- error.  The [Kahan summation
-- algorithm](https://en.wikipedia.org/wiki/Kahan_summation_algorithm)
-- improves the accuracy over naive summation by tracking an
-- additional *error term*, which collects small values that would
-- otherwise be lost.

type kahan = (f64, f64)

def kahan_add ((s1, c1) : kahan) ((s2, c2) : kahan) : kahan =
  let s = s1 + s2
  let d  = if f64.abs s1 >= f64.abs s2 then (s1 - s) + s2
           else (s2 - s) + s1
  let c = (c1 + c2) + d
  in (s, c)

-- The above actually incorporates [a tweak by
-- Neumaier](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
-- that better handles the case where the term to be added is larger
-- than the running sum.  We can pack it up as a `map`-`reduce`
-- composition for summing an entire array.

def kahan_sum (xs: []f64) : f64 =
  let (s,c) = reduce kahan_add (0,0) (map (\x -> (x,0)) xs)
  in s + c

-- And it really is more accurate than normal summation, as easily
-- shown with some pathological examples:

-- > kahan_sum [1e100, 1.0, -1e100, 1.0]

def normal_sum = f64.sum

-- > normal_sum [1e100, 1.0, -1e100, 1.0]

-- Is `kahan_add` actually associative, as required by `reduce`?  No,
-- it's not, but neither is floating-point addition in the first
-- place.  Passing a non-associative operator to `reduce` doesn't mean
-- the result can be arbitrary, it just means the result is
-- nondeterministic, based on how the compiler or runtime decides to
-- "parenthesise" the application of the operator.  We're already
-- implicitly accepting this when we parallelise a floating-point
-- summation, so we should be fine with it for Kahan summation as
-- well.
