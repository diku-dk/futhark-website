-- # Triangular matrices
--
-- A triangular matrix is a kind of sparse square matrix where all
-- elements above or below the diagonal are zero.  In the former case,
-- we call it a *lower triangular matrix*.  While we can always
-- represent an *n²* triangular matrix as an ordinary *n²* matrix
-- where we store the zeroes, this is wasteful of memory.  Instead, we
-- can use a representation where we store only the possibly nonzero
-- elements.  Since Futhark only supports regular arrays, we must
-- encode this as a one-dimensional array.  To hide the complexity of
-- the encoding from the user, we define triangular matrices as an
-- [abstract data type](abstract-data-types.html).

-- We start out by defining a module type describing a minimal
-- interface for lower triangular matrices.

module type triangular = {

-- The type of triangular matrices is parameterised by the edge length
-- and the element type.

  type~ triangular [n] 'a

--  We define a *size-lifted type* with `type~` for technical reasons, as our
-- implementation will eventually contain an array whose size is not
-- literally `n`.  The Futhark type checker is very rigid, and does
-- not allow us to "hide" arrays of sizes that are not present in the
-- type.

  val get [n] 'a :
    a -> (i64,i64) -> triangular [n] a -> a

-- `get zero (i,j) m` should produce the element at logical position
-- `(i,j)` in the triangular matrix `m`, returning `zero` whenever we
-- are above the diagonal.  The explicit `zero` is needed because our
-- type is fully polymorphic - we have no idea what `a` this is.

  val from_array [n] 'a :
    [n][n]a -> triangular [n] a

-- The `from_array` function constructs a triangular matrix from a
-- dense array.  All elements above the diagonal are ignored.
--
-- We also provide a way to convert a triangular matrix back into a
-- dense array.  The caller must provide a zero value, for the same
-- reason as with `get` above:

  val to_array [n] 'a :
    a -> triangular [n] a -> [n][n]a

-- Finally, as an example of a higher-order operation, we provide a
-- `map` function for triangular matrices:

  val map [n] 'a 'b :
    (a -> b) -> triangular [n] a -> triangular [n] b

-- This is a very sparse interface, and would probably need to be
-- extended if we wanted to use it in real applications.

}

-- Now let's implement that module type.

module triangular : triangular = {

-- First thing, we define our representation of triangular arrays.
-- What we *need* is a one-dimensional array to store the elements
-- (the `data` array).  However, whenever we have a type with a size
-- parameter (the `[n]`), then we *must* also have an array of that
-- size in the definition of type type.  The `data` array will not
-- have the right size, so we add a size-`n` array of empty tuples.
-- In effect, the `size` field acts acts as a "witness" for the size
-- parameter.  Hopefully, the run-time representation for this array
-- of empty values will be just its size.

  type~ triangular [n] 'a =
    { size: [n](),
      data: []a
    }

-- The size of the `data` array given `n` is given by this formula:

  let elements (n: i64) =
    (n * (1+n))/2

-- We now define a handful of utility functions for converting between
-- *flat* indexes into the `data` array and the two-dimensional
-- indexes that we use for ordinary arrays.  For *n=4*, the indexes of
-- the elements are as follows:
--
-- ```
-- 0
-- 1 2
-- 3 4 5
-- 6 7 8 9
-- ```
--
-- First a function that determines which row a *flat* index into the
-- `data` array conceptually belongs to.  Don't worry about the opaque
-- formula - it's essentially just a solution to a certain
-- second-degree equation (I'll admit it's a bit odd to see square
-- roots in index calculations).

  let row (i: i64) =
    i64.f64 (f64.ceil ((f64.sqrt(f64.i64(9+8*i))-1)/2))-1

-- It behaves like this:
--
-- ```
-- > row 0
-- 0i64
-- > row 1
-- 1i64
-- > row 2
-- 1i64
-- > row 3
-- 2i64
-- ```

-- Now can define *ranking*, which turns a two-dimensional index
-- *(i,j)* into a flat index:

  let rank i j =
    elements i + j

-- And the other way around:

  let unrank (p: i64) =
    let i = row p
    let j = p - elements i
    in (i,j)

-- Finally we can define the API functions, which are mostly
-- straightforward.  First `get`, where we manually check whether we
-- are trying to index above the diagonal, and return the zero value
-- if so:

  let get [n] 'a zero (i,j) (tri: triangular [n] a) =
    if j > i then zero else tri.data[rank i j]

-- Converting between ordinary arrays and triangular arrays is also
-- straightforward.

  let from_array arr =
    { size = map (const ()) arr,
      data = map (\p -> let (i,j) = unrank p
                        in arr[i,j])
                 (iota (elements (length arr)))
    }

  let to_array [n] 'a zero (tri: triangular [n] a) =
    tabulate_2d n n (\i j -> get zero (i,j) tri)

-- In fact, `to_array` does not have to be part of the module, as it
-- is defined in terms of the standard prelude function `tabulate_2d`,
-- and the exposed `get` function.
--
-- Finally, defining `map` is very simple, as it doesn't have to care
-- about the sizes at all.

  let map f {size, data} =
    {size, data = map f data}

-- And we're done!

}

-- This module is probably too small to be useful.  In practice, I'd
-- expect we'd also need `zip` functions as a minimum, and some kind
-- of `reduce` also seems like it might be useful.  We can always use
-- `from_array` and `to_array` to convert back and forth between
-- ordinary arrays as needed, though.
