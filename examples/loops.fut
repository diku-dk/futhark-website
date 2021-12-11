-- # Loops
--
-- Futhark does not directly support recursive functions, but instead
-- provides syntax for expressing sequential loops.

def x =
  loop acc = 0 for i < 10 do
    acc * 2 + i

-- In this loop, `acc` is the *loop parameter*, which is initialised
-- to the value 0.  We then evaluate the *loop body* `acc * 2 + i` ten
-- times with `i` bound to 0, 1, ..., 9.  The loop body returns a new
-- value for `acc`.  At the end of the final iteration, the loop
-- parameter is returned and bound to `x` with a normal `let` binding.
--
-- The loop parameter can be any pattern:

def xy =
  loop (x, y) = (1,1) for i < 10 do
    (y, x + y)

-- The result of this loop is a pair.  We could also have written it
-- as follows:

def xy' =
  loop p = (1,1) for i < 10 do
    (p.1, p.0 + p.1)

-- Apart from `for` loops, Futhark also supports `for-in` loops:

def max =
  loop cur = 0 for x in [4,7,4,8,2,6,4,5] do
    if x > cur then x else cur

-- And `while` loops:

def res =
  loop (i, acc) = (0, 53) while acc > 0 do
    (i + 1, acc / 2)

-- `loop` results in entirely sequential execution.  In most cases
-- it's better to use a parallel operation like `map`, `reduce`, or
-- `scan`.
