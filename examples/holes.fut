-- # Holes
--
-- A *hole* is a placeholder expression we can use when we don't yet
-- know what to put:

def foo [n] (x: [n]f32) : i64 = ???

-- The type checker will issue a warning for each hole encountered,
-- listing the inferred type:
--
-- ```
-- Warning at holes.fut:6:28-30:
--   Hole of type: i64
-- ```
--
-- Holes can have any type, including functions:

def bar [n] (x: [n]f32) : i64 = ??? x

-- Which will warn us:
--
-- ```
-- holes.fut:18:33-35:
--   Hole of type: [n]f32 -> i64
-- ```
--
-- Hole expressions make it possible to sketch out the design of a
-- program by defining only the types of top-level functions, and then
-- gradually fill in the blanks.  We can also compile and run a
-- program with holes in it, but executing a hole results in a
-- run-time error.
--
-- Note that because holes are subject to ordinary type inference,
-- they must have an unambiguous type.  For example, the expression
-- `length ???` will fail to type-check because the type checker has
-- no way to determine the element type or size of the placeholder
-- array.  You can use type ascriptions to disambiguate such cases, if
-- necessary:

def baz n = length (??? : [n]i32)

-- # See also
--
-- [Type ascriptions](type-ascriptions.html)
