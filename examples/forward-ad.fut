-- # Forward-mode automatic differentiation
--
-- The built-in `jvp` function computes the product of a *seed vector*
-- and the
-- [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
-- of a function at some point.  This can be used to compute
-- derivatives of functions.

def f x = f64.sqrt(x) * f64.sin(x)

def f' x = jvp f x 1

-- > f' 2f64

-- `jvp` corresponds to differentiation by [forward
-- accumulation](https://en.wikipedia.org/wiki/Automatic_differentiation#Forward_accumulation),
-- and is most efficient for functions that have more outputs than
-- inputs.  For a function with multiple inputs, you need multiple
-- applications of `f'` to compute the full derivative, each with a
-- [one-hot](https://en.wikipedia.org/wiki/One-hot) seed vector:

def g (x,y) = f64.cos(x) * f64.sin(y)

def g' x y = (jvp g (x,y) (1,0), jvp g (x,y) (0,1))

-- > g' 1f64 2f64

-- ## See also
--
-- [Reverse-mode automatic differentiation](reverse-ad.html).
