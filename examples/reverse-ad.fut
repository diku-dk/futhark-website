-- # Reverse-mode automatic differentiation
--
-- The built-in `vjp` function computes the product of the
-- [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
-- of a function at some point and a *seed vector*.  This can be used
-- to compute derivatives of functions.

def f x = f64.sqrt(x) * f64.sin(x)

def f' x = vjp f x 1

-- > f' 2f64

-- `jvp` corresponds to differentiation by [reverse
-- accumulation](https://en.wikipedia.org/wiki/Automatic_differentiation#Reverse_accumulation),
-- and is most efficient for functions that have more inputs than
-- outputs.  A particularly important special case of these are [loss
-- functions](https://en.wikipedia.org/wiki/Loss_function) that return
-- just a single scalar.  For these we can define a function for
-- finding gradients:

def grad f x = vjp f x 1f64

def g (x,y) = f64.cos(x) * f64.sin(y)

def g' x y = grad g (x,y)

-- > g' 1f64 2f64

-- ## See also
--
-- [Forward-mode automatic differentiation](forward-ad.html),
-- [Newton's Method](newton-ad.html).
