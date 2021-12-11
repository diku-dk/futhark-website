-- # Moving average
--
-- We can compute the average value of an array by summing the
-- elements, then dividing by the array size:

def avg [n] (xs: [n]f64) : f64 =
  f64.sum xs / f64.i64 n

-- This is specialised to arrays with `f64` elements.  We could use
-- the module system to abstract over the element type, but we'll
-- stick with the monomoprhic case for simplicity.
--
-- Now consider computing the [moving
-- average](https://en.wikipedia.org/wiki/Moving_average).  For each
-- element of the input, we compute the average of a *window*
-- consisting of the preceding and succeeding `m` elements.  This can
-- be implemented straightforwardly in Futhark, but we have to be
-- careful near the edges of the array, where a full window may not be
-- available:

def movavg [n] (m: i64) (xs: [n]f64) : [n]f64 =
  tabulate n (\i ->
                let start = i64.max 0 (i-m)
                let end = i64.min n (i+m+1)
                let window = xs[start:end]
                in avg window)

-- This implementation *works*, but with a caveat.  The problem is
-- that the size of the `window` slice is not the same for all values
-- of `i`.  This is an instance of *irregular nested parallelism*,
-- which is in general not handled well by the Futhark compiler.  In
-- this particular case, there is no practical difference, but for
-- completeness, here's how you'd implement it with only regular
-- nested parallelism:

def movavg_regular [n] (m: i64) (xs: [n]f64) : [n]f64 =
  let full_wsize = 2*m+1
  let in_bounds i = i >= 0 && i < n
  in tabulate n (\i ->
                   let indices = tabulate full_wsize (\j -> i+j-m)
                   let indices_in_bounds = map in_bounds indices
                   let wsize = i64.sum (map i64.bool indices_in_bounds)
                   let window = map2 (\j b -> if b then xs[j] else 0)
                                     indices indices_in_bounds
                   in f64.sum window / f64.i64 wsize)

-- The idea is to *always* compute a window array of `full_wsize`
-- elements, using zeroes when we would otherwise go out of bounds.
-- All the nested parallel operations will be on arrays of this size.
-- The main complication is that we need to divide the window sum with
-- the "real" window size, not the padded one.
