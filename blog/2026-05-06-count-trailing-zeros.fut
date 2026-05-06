-- ---
-- title: Count trailing zeros
-- description: A fun little parallel programming problem, complete with property-based testing.
-- ---
--
-- In a [recent episode of
-- ArrayCast](https://www.arraycast.com/2026/04/06/Episode-123.html), Adam talks
-- about counting trailing ones in an array in APL, which can be done rather
-- easily with a trick. That got me thinking about how to do it in Futhark, but
-- during my thoughts I made an off-by-one error, so I ended up implementing how
-- to count trailing *zeros* instead. The approach I took to my solution is
-- quite similar to the one explained in a [previous
-- post](2023-01-27-gccontent.html), but this is a bit simpler.

-- ## Hacking
--
-- To be clear, we want to define a function that works like this:

-- > trailing_zeros [1,2,3]

-- > trailing_zeros [1,2,3,0,0]

-- > trailing_zeros [1,2,3,0,0,1]

-- Many of these problems can be solved with a divide-and-conquer approach,
-- which can be done in a nice way using data parallel programming. We start by
-- defining a data type that represents the result for some subsequence of the
-- input, which usually contains the result we actually care about (the number
-- of traiing zeros in the subsequence), as well as some auxiliary data, in this
-- case the length of the subsequence.

type result = {len: i64, trailing: i64}

-- Then we define the result of an *empty* sequence:

def empty : result = {len = 0, trailing = 0}

-- And the result for a single element:

def singleton (x: i32) : result =
  {len = 1, trailing = if x == 0 then 1 else 0}

-- We then define a function for combining the result of two *adjacent*
-- subsequences into a single result. The logic is as follows:
--
-- 1. If the right subsequence is entirely zeros, then we add the number of
--    trailing zeros in the left subsequence.
--
-- 2. Otherwise, the result is just the same as for the right subsequence, as
--    the zeros in the left one cannot possibly be trailing.

def combine (a: result) (b: result) : result =
  if b.len == b.trailing
  then { len = a.len + b.len
       , trailing = a.trailing + b.trailing
       }
  else b

-- This function is actually associative, and has `empty` as its neutral
-- element. This satisfies the requirements for performing a
-- [reduction](../examples/scan-reduce.html):

def trailing_zeros (xs: []i32) =
  (reduce combine empty (map singleton xs)).trailing

-- And now we have a parallel function for counting trailing zeros! This is an
-- example of solving a problem with a [list
-- homomorphism](https://sigkill.dk/writings/par/lhomo.html), and is a technique
-- that can be applied whenever we can conceive of a solution based on splitting
-- a list at some arbitrary point, computing subresults for the chunks, and
-- combining the subresults (at least whenever the combining operator is
-- associative).

-- ## Testing
--
-- But hang on, can we be sure that this is right? Let us use the [in-progress
-- work on property-based
-- testing](https://github.com/diku-dk/futhark/pull/2405), which was [discussed
-- in a recent episode](2026-03-25-property-based-testing.html), to write down a
-- property for this function.
--
-- The first thing we need to do is write a *generator* that can construct
-- random input values. To increase the odds of getting zeros in the input, we
-- heavily bound the range of integers. In the current design, a generator is a
-- function that accepts a size and a seed, so we can define one that generates
-- random arrays of 32-bit integers like so:

entry gen_i32_array (size: i64) (seed: i32) : []i32 =
  let hash (x: i32): i32 =
    let x = u32.i32 x
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x)
    in i32.u32 x
  in tabulate size (\i -> hash (seed ^ i32.i64 i) % 3)

-- Obviously we want to construct a library of generators so you don't have to
-- write boilerplate like this quite so often.
--
-- Similarly, a property is an entry point function with an attribute connecting
-- it to a generator. Our property for `trailing_zero`s checks for the
-- following:
--
-- 1. The count `k` must nonzero and at most the length of the arrays.
--
-- 2. The length `k`-suffix of the input must contain only zeros.
--
-- 3. The element preceding the suffix (if any) must be nonzero.
--
-- It is defined like this:

#[prop(gen(gen_i32_array))]
entry prop_trailing_zeros (xs: []i32) : bool =
  let k = trailing_zeros xs
  in k >= 0 && k <= length xs
     && all (== 0) (take k (reverse xs))
     && (k == length xs || (reverse xs)[k] != 0)

-- And then we just tell the testing tool that there is a property worth
-- testing:

-- ==
-- property: prop_trailing_zeros

-- Now `futhark test 2026-05-06-count-trailing-zeros.fut` will run the test for
-- our function, as [this blog post](2026-05-06-count-trailing-zeros.fut) is
-- indeed a [literate Futhark](2021-01-18-futharkscript.html) program! The
-- output is not particularly interesting, merely stating that all tests pass.
-- Maybe it would be nice if the tool could produce a log of all attempted
-- inputs. Still, it's nice that we will soon have a tool that can automate the
-- otherwise manual process of verification by running a function of a handful
-- of inputs.
