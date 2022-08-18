-- # Variance
--
-- A naive way of computing
-- [variance](https://en.wikipedia.org/wiki/Variance) of an array
-- where each element is considered equally likely is as follows,
-- which is close to the textbook formula:

def mean [n] (vs: [n]f64) =
  f64.sum vs / f64.i64 n

def variance [n] (vs: [n]f64) =
  let m = mean vs
  let xs = map (\x -> (x-m)*(x-m)) vs
  in f64.sum xs / (f64.i64 n)

-- Unfortunately, sums of squares can lead to numerical instability.
-- A parallel adaptation of [Welford's Online
-- Algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm)
-- can be used to obtain a more stable variance.  The basic idea is a
-- divide-and-conquer parallel algorithm where a subsequence of the
-- data is described by a triple containing:
--
-- 1. The length of the subsequence.
-- 2. The mean of the subsequence.
-- 3. The estimated variance of the subsequence.
--
-- If we have an associative operator for combining two such triples,
-- then we can write it as a `map`-`reduce` composition in Futhark.
-- Fortunately, such an operator exists:

def red_op (n_a, m_a, m2_a) (n_b, m_b, m2_b) =
  let n_ab = n_a + n_b
  in if n_ab == 0 then (0, 0, 0)
     else let f_a = f64.from_fraction n_a n_ab
          let f_b = f64.from_fraction n_b n_ab
          let f_ab = f64.from_fraction (n_a * n_b) n_ab
          let delta = m_b - m_a
          let m_ab = f_a * m_a + f_b * m_b
          let m2_ab = m2_a + m2_b + delta * delta * f_ab
          in (n_ab, m_ab, m2_ab)

-- The only real subtlety is that we need to special-case a length of
-- zero to avoid division by zero.  We can now define a numerically
-- stable function for computing variance.

def variance_stable [n] (X:[n]f64) =
  let (_, _, m2) =
    reduce_comm red_op (0, 0, 0) (map (\a -> (1, a, 0)) X)
  in (m2/f64.i64 n)

-- Note also that this function is a single-pass algorithm.
--
-- Thanks to [Gusten
-- Isfeldt](https://treesearch.se/en/researchers/gusten-isfeldt/) for
-- this example.
