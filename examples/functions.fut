-- # Functions
--
-- Functions are defined with `let`, where we can also annotate both
-- the parameter and return types.

let plus2 (x: i32) : i32 =
  x + 2

-- Type inference is supported, so in many cases the types can be left
-- off, in which case the correct type will be inferred from the
-- definition.

let plus2_inferred x =
  x + 2

-- Functions are (almost) first-class values, with the type of a
-- function from type `a` to type `b` written as `a -> b`.  This means
-- we can write higher-order functions:

let on_i32 (f: i32 -> i32) (x: i32) =
  f x

-- Function values are restricted as follows:
--
-- * Arrays of functions are not permitted.
--
-- * A function cannot be returned from an `if` expression.
--
-- * A [loop parameter](loops.html) cannot be a function.
--
-- However, it is fine to collect functions in tuples or records.

let fun_tuple : (i32 -> i32, i32 -> i32) =
  (plus2, plus2_inferred)

-- [Polymorphic functions are also
-- supported](parametric-polymorphism.html).
