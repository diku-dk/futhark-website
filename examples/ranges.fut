-- # Ranges
--
-- Range expressions produce arrays of integers from some start up to
-- (or down to) some limit:

def ints = 1..<10

-- > ints

-- In the expression above, the upper limit is *not* included.
-- Inclusive ranges are also supported:

def ints_inclusive = 1...10

-- > ints_inclusive

-- By default the distance between each number is 1 (the *stride*).
-- This can be changed by providing an explicit second element:

def ints_by_two = 1..3..<10

-- > ints_by_two

-- If we want a descending range, we *must* provide a second element:

def ints_descending = 10..9...0

-- > ints_descending

-- An exclusive descending range is also possible:

def ints_descending_exclusive = 10..9..>0

-- > ints_descending_exclusive

-- ## Hints
--
-- The predefined functions `iota` and `indices` encapsulate common
-- usages of ranges.
--
-- Instead of `0..<x`, use `iota x`.
--
-- Instead of `iota (length A)` (or `0..<length A`), use `indices A`.

-- ## See also
--
-- [Complex ranges](complex-ranges.html)
