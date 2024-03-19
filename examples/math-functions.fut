-- # Mathematical functions
--
-- Builtin mathematical functions such as trigonometrical functions,
-- square roots, and so on, are available through built-in modules.

def hypo (x: f32) (y: f32) = f32.sqrt (x*x + y*y)

-- A module exists for every number type.

-- ## See also
--
-- [The builtin mathematical modules.](../docs/prelude/doc/prelude/math.html)
--
-- [Three-dimensional vectors](Three-dimensional vectors), [Complex numbers](complex-numbers.html).
