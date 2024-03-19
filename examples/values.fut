-- # Values
--
-- Futhark has various primitive types: eight different types of
-- integers (8 to 64 bit, both signed and unsigned), single and double
-- precision floats, and booleans. Here are some examples.

def an_integer : i32 = 2

def an_unsigned_byte : u8 = 2

def a_boolean = true

def a_double : f64 = 2.0

-- Numeric literals are overloaded, such that `2` can be any numeric
-- type, and decimal literals like `2.0` must be either `f32` or
-- `f64`.

-- ## See also
--
-- [Arrays](arrays.html), [Tuples and records](tuples-and-records.html), [Mathematical functions](math-functions.html).
--
-- Reference manual: [Primitive types and values](https://futhark.readthedocs.io/en/stable/language-reference.html#primitive-types-and-values).
