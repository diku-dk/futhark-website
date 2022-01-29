-- # Matrix multiplication
--
-- Futhark has no builtin operator or function for multiplying
-- matrices.  Indeed, it does not have matrices as a distinct type at
-- all.  Instead, matrices are represented simply as two-dimensional
-- arrays of some type that supports multiplication and addition.  We
-- can write a function for multiplying integer matrices via the usual
-- `map`/`reduce` constructs:

def matmul_i32 [n][m][p] (A: [n][m]i32) (B: [m][p]i32) : [n][p]i32 =
  map (\A_row ->
         map (\B_col ->
                reduce (+) 0 (map2 (*) A_row B_col))
             (transpose B))
      A

-- We can also write a [polymorphic higher-order
-- function](parametric-polymorphism.html) that encapsulates the
-- general pattern:

def matmul [n][m][p] 'a
           (add: a -> a -> a) (mul: a -> a -> a) (zero: a)
           (A: [n][m]a) (B: [m][p]a) : [n][p]a =
  map (\A_row ->
         map (\B_col ->
                reduce add zero (map2 mul A_row B_col))
             (transpose B))
      A

-- We can then partially apply `matmul` to obtain a matrix
-- multiplication function for a specific type.

def matmul_f32 = matmul (+) (*) 0f32

-- # See also
--
-- [Outer product](outer-product.html).
