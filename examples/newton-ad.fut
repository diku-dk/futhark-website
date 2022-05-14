-- # Newton's Method using Automatic Differentiation
--
-- [Newton's Method](https://en.wikipedia.org/wiki/Newton%27s_method)
-- is a numerical algorithm for finding roots of real-valued
-- functions.  It requires us to know the derivative of the function,
-- which we use Futhark's support for automatic differentiation to
-- obtain.

def newton (tol: f64) (f: f64 -> f64) (x0: f64) =
  let iteration (_, x, i) =
    let (y, dy) = jvp2 f x 1
    let x' = x - y / dy
    in (f64.abs (x - x') < tol, x', i+1)
  let (_, x, steps) = iterate_until (.0) iteration (false, x0, 0)
  in (x, steps)

-- The `jvp2` function is like `jvp` (see [reverse-mode automatic
-- differentiation](reverse-ad.html)), but also returns the normal
-- result of the function.
--
-- To use `newton`, we first define an appropriate objective function,
-- in this case a Cubic equation with the roots 1, pi, and 42:

def f x : f64 = (x - 1) * (x - f64.pi) * (x - 42)

def f_roots = newton f64.epsilon f

-- The number of steps needed to find a root depends on the initial
-- guess of `x0`:

-- > f_roots 0.0

-- > f_roots 4.0

-- > f_roots 43.0

-- > f_roots 1000000000.0

-- Thanks to [Gusten
-- Isfeldt](https://treesearch.se/en/researchers/gusten-isfeldt/) for
-- this example.
