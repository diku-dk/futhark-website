-- ---
-- title: Converting a value to a different type
-- ---
--
-- To convert a value of type `i64` to `f32`, use the function `f32.i64`:

let x : i64 = 2
let y : f32 = f32.i64 x

-- Generally, to convert a value of builtin type `F` to builtin type
-- `T`, use the function `T.F`:

let z : bool = bool.i64 x

-- This is not special syntax, but simply calling the function `i64`
-- in module `bool`.  It just so happens that the prelude has modules
-- defined with the same name as each of the builtin scalar functions,
-- and that these modules contain similarly named conversion
-- functions.
--
-- Conversion to a type with smaller size will truncate if needed.
-- Floats converted to integers are rounded toward zero.
--
-- Converting an integer type to a larger signed integer type
-- will perform sign extension.
--
-- Converting an integer type to a larger signed integer type
-- will perform zero extension.
--
-- # See also
--
-- [The prelude file defining these
-- modules](https://futhark-lang.org/docs/prelude/doc/prelude/math.html),
-- specifically the `from_prim` module type.
