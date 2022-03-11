-- # Shape coercions

-- The Futhark type checker is not very smart, and in some cases is
-- very conservative in how it tracks the sizes of arrays.  For
-- example:

def A = [1,2,3] ++ [4,5,6]

-- As far as the type checker is concerned, `A` has a completely
-- unknown size.  Using a *shape coercion* we can change the size to be
-- what we expect:

def B = [1,2,3] ++ [4,5,6] :> [6]i32

-- Shape coercions are checked dynamically, and the program will fail
-- at run-time if the actual shape does not match the expected shape.
-- The rank or element type of the array may not change - only the
-- specific dimensions.

-- # See also
--
-- [Complex ranges](complex-ranges.html)
