-- # Comparing floating-point numbers for equality
--
-- Comparing floating-point numbers for absolute equality with `==` is often
-- inappropriate because of the risk of rounding errors, particularly when doing
-- parallel summation using strictly speaking non-associative floating-point
-- addition. A common solution is to use a function that checks approximate
-- equality, using both a relative and an absolute tolerance:

def approx_eql (rel_tol: f64) (a: f64) (b: f64) : bool =
  let diff = f64.abs (a - b)
  let scale = f64.max (f64.abs a) (f64.abs b)
  let abs_tol = 100.0 * f64.epsilon * scale
  in diff <= f64.max abs_tol (rel_tol * scale)

-- > approx_eql 1e-9 1.0 1.0000000005
