-- # Comparing arrays for equality

-- To compare two one-dimensional arrays for equality, we can compare
-- them elementwise, yielding an array of `bool`s, then use `and` to
-- check whether all those elements are true:

def eq_arr_i32 [n] (a: [n]i32) (b: [n]i32) : bool =
  and (map2 (==) a b)

-- This function requires us to already know that the arrays have the
-- same size. We can also write a variant that does not have this
-- requirement, but instead checks for the size explicitly and then
-- uses a [size coercion](size-coercions.html):

def eq_arr_i32_any [n] [m] (a: [n]i32) (b: [m]i32) : bool =
  n == m && eq_arr_i32 a (b :> [n]i32)

-- This implementation is still restricted to arrays of integers. We
-- can make a polymorphic one that expects the user to provide an
-- equality function:

def eq_arr 'a [n] [m] (eq: a -> a -> bool) (a: [n]a) (b: [m]a) : bool =
  n == m && and (map2 eq a (b :> [n]a))

-- This can then be used to define a function that works for
-- two-dimensional arrays:

def eq_arr_2d 'a [n1] [n2] [m1] [m2]
              (eq: a -> a -> bool) (a: [n1][n2]a) (b: [m1][m2]a) : bool =
  n1 == m1 && n2 == m2 && eq_arr (eq_arr eq) a (b :> [n1][n2]a)

-- ## See also
--
-- [Searching](searching.html).
