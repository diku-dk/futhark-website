-- # Polymorphic minimum and maximum
--
-- Futhark provides [built-in functions](minmax.html) for computing the minimum
-- and maximum element of arrays of built-in types. The following functions are
-- generic implementations that work for any type, as long as you provide a
-- comparison operator.

def minimum [n] 't ((<=): t -> t -> bool) (a: [n]t) : t =
  reduce (\x y -> if x <= y then x else y) a[0] a

def maximum [n] 't ((<=): t -> t -> bool) (a: [n]t) : t =
  minimum (flip (<=)) a

-- We can instantiate them to obtain the normal functions.

def minimum_i32 = minimum (i32.<=)

-- > minimum_i32 [1,2,3,-4,5]

-- The functions above are not perfect, as they require the element to be
-- non-empty. One could conceive of variants that use [option types](opt.html)
-- to handle empty arrays. However, it is most efficient if we can provide a
-- sensible neutral/dummy element for the reduction.

-- ## See also
--
-- [Index of smallest element (argmin)](argmin.html), [Option types](opt.html),
-- [Reducing or scanning without a neutral element](no-neutral-element.html).
