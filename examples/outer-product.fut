-- # Computing outer products
--
-- The [outer product](https://en.wikipedia.org/wiki/Outer_product) of
-- two arrays can be defined as follows:

def outer_prod op A B =
  map (\a -> map (\b -> a `op` b) B) A

-- This definition is parameterised over the multiplication operator.
-- This allows us to obtain the "standard" outer product:

let std_outer_prod = outer_prod (f64.*)

-- > std_outer_prod [1.0,2.0,3.0] [4.0,5.0,6.0]

-- But we can also use to define matrix multiplication:

def dotprod A B =
  f64.sum (map2 (*) A B)

def matmul A B =
  outer_prod dotprod A (transpose B)

-- > matmul [[1.0,2.0],[3.0,4.0],[5.0,6.0]] [[7.0,8.0,9.0],[10.0,11.0,12.0]]
