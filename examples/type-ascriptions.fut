-- # Type ascriptions
--
-- A type ascription is a way of telling the type checker the expected
-- type of some expression.

def foo = 1 : f64

-- It is used to disambiguate otherwise ambiguous cases.  For example,
-- the literal `1` above would be inferred to have type `i32` if not
-- for the type ascription.
--
-- For return types, we put the ascription on the definition itself:

def bar : f64 = 1

-- This is equivalent to the above, but more common.
--
-- Most Futhark programs do not need explicit type ascriptions
-- (although it is good form to list return types), but it can be
-- needed to disambiguate some tricky cases.  For example, if we want
-- to create an empty two-dimensional array with a specific inner size:

def baz = [] : [0][10]f32

-- Type ascriptions are not "casts" and cannot change the type of a
-- value.
--
-- # See also
--
-- [Converting a value to a different type](converting.html), [Shape
-- coercions](size-coercions.html).
