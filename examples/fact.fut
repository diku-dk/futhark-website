-- # Basic usage with the factorial function
--
-- The [factorial](https://en.wikipedia.org/wiki/Factorial) of a
-- number *n*, written *n!*, is the product of all integers from 1 to
-- *n* inclusive.  It is often used as the *hello world* program of
-- functional languages.  Usually, the given definition is recursive,
-- but Futhark does not support recursion, so instead we use
-- `reduce`:

let fact (n: i32): i32 = reduce (*) 1 (1...n)

-- If we want this function to be usable from the outside world, we
-- need to define an entry point that calls it.  By default, any
-- function by the name `main` is an entry point:

let main (n: i32): i32 = fact n

-- The function call `fact n` creates an array of the integers
-- `1...n`, then computes the product of all elements in the array.
-- The Futhark compiler employs *loop fusion* to remove the need for
-- the intermediate array to be actually created.  Technically, `fact
-- n` does not compute *n!*, but rather *n! mod 2**32*, as `i32`s are
-- 32 bit in size and will rapidly overflow for large *n*.
--
-- If we put the above program in a file `fact.fut`, we can compile it
-- using the OpenCL backend as such:
--
-- ```
-- $ futhark opencl fact.fut
-- ```
--
-- If all goes well, this produces an executable program `fact` in the
-- current directory.  Similarly, we can compile to sequential C code
-- with `futhark c`.  Futhark is not intended to be used for writing
-- standalone programs, but it is supported in order to enable testing
-- and benchmarking.  A standalone program will expect to be given its
-- arguments on standard input::
--
-- ```
-- $ echo 2000000000 | ./fact
-- ```
--
-- This will write the result on standard output (`0i32` - a 32-bit
-- zero).
