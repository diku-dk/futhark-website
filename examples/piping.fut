-- ---
-- title: Pipe operators
-- ---
--
-- Futhark programs are typically written as chains of functions that
-- each slightly massage the data somehow.  For example, to compute
-- the sum of square roots of all positive numbers in an array:

let square_pos xs =
  f64.sum (map f64.sqrt (filter (>0) xs))

-- Note that [this is not the best way to write this
-- function](filter-reduce.html).  Also, These chains can become
-- unwieldy as they grow.  Partly because of the parentheses, and
-- partly because they have to be read right-to-left.  The *pipe
-- forward operator* `|>` lets us write it like this instead:

let square_pos_pipe xs =
  xs |> filter (>0) |> map f64.sqrt |> f64.sum

-- Now the dataflow is purely left-to-right and we can easily add more
-- links in the processing chain.
--
-- There is nothing magical in the pipe operator.  It's a polymorphic
-- higher-order function that you could have defined yourself, but
-- happens to be predefined in the Futhark
-- [prelude](https://futhark-lang.org/docs/prelude/doc/prelude/prelude.html/).
-- It's defined like this:
--
-- ```
-- let (|>) '^a '^b (x: a) (f: a -> b): b = f x
-- ```
--
-- There is also a *pipe backwards operator* `<|`, but due to
-- Futhark's restrictive size type system, it has some nonintuitive
-- limitations, and usually does not work with functions that return
-- arrays of statically unknown size (such as `filter`).

-- # See also
--
-- Reference manual: [Size
-- types](https://futhark.readthedocs.io/en/latest/language-reference.html#size-types)
