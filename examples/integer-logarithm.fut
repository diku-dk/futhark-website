-- # Integer logarithm
--
-- The base-2 integer logarithm can be computed by counting the number
-- of leading *zero bits* in the integer, through the `clz` builtin
-- function. For example for 64-bit integers:

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

-- > ilog2 127

-- > ilog2 128

-- ## See also
--
-- [Merge sort](merge-sort.html).
