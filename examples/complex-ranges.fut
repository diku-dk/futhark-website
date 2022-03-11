-- # Complex ranges
--
-- While [range expressions](ranges.html) can be useful, the type
-- checker is unable to deduce the size of nontrivial ranges.

def ints10 = 0..<10

def ints5 = 0..2..<10

-- While the type checker knows that `ints10` has 10 elements, `ints5`
-- is assigned an "unknown" size.  In some cases this does not matter,
-- but for example, you cannot `zip` it with another array.

-- [Shape coercions](shape-coercions.html) can be used to address
-- this, but sometimes awkwardly.  In most cases it is better to
-- create a range where the size is obvious to the type checker, and
-- then massage it to the desired value using `map`:

def ints5_alternative = map (*2) (0..<5)
