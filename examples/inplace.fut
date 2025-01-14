-- # In-place updates
--
-- Futhark supports a mechanism for changing the contents of an array
-- without copying the entire array.

def update (A: *[]i32) (i: i32) (v: i32) =
  A with [i] = v

-- > update [1,2,3] 0 42

-- The `*` annotation for the `A` parameter indicates that the
-- `update` function *consumes* the array, meaning the argument value
-- may not be used again by the caller. This is checked through
-- Futhark's [uniqueness type
-- system](https://futhark.readthedocs.io/en/latest/language-reference.html#in-place-updates).
--
-- We can also create a variant of `update` that does not consume the
-- array, at the cost of always copying it.

def update_copy (A: []i32) (i: i32) (v: i32) =
  copy A with [i] = v

-- This is observably equivalent to `update`.

-- > update_copy [1,2,3] 0 42

--
-- ## See also
--
-- [Swapping two elements of an array](swap.html).
