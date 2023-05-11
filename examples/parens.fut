-- # Matching parentheses
--
-- Consider finding pairs of balanced parentheses in a string such as
-- `"()(()())((()))"` consisting entirely of open and closing
-- parentheses.  This problem has an elegant parallel solution.  We'll
-- assume that the string is indeed balanced - *checking* whether that
-- is the case, or also allowing non-parenthesis characters, is a
-- fairly easy extension.
--
-- First we define a function for finding the nesting depth of each
-- character.

def depths [n] (s: [n]u8) : [n]i32 =
  let depth = scan (+) 0 (map (\c -> if c == '(' then 1 else -1) s)
  in map2 (\c d -> d - if c == '(' then 1 else 0) s depth

-- > depths "()(()())((()))"

-- Next, we define a function that gives the sorting order of an array
-- of integers.  Although we will use a [previously defined radix
-- sort](radix-sort-key.html) as a building block, this is not the
-- same as a sorted array!  We use the name `grade` for this function,
-- [inspired by APL](https://aplwiki.com/wiki/Grade):

import "radix-sort-key"

def grade xs =
  zip (map u32.i32 xs) (indices xs)
  |> radix_sort (.0)
  |> map (.1)

-- For an array `A`, the result `grade A` tells us the order in which
-- to index `A` to obtain the sorted array.

-- > grade [5,0,1,3,2,4]

-- Finally, with the help of a small auxiliary function for permuting
-- arrays, we can finish our implementation of parentheses matching.

def permute xs is = scatter (copy xs) is xs

def match_parens [n] (s: [n]u8) =
  let depth = depths s
  let rnk = grade depth
  let ret = tabulate n (\i -> if i % 2 == 0 then rnk[i+1] else rnk[i-1])
  in permute ret rnk

-- The function returns, for each parenthesis in the input string, the
-- index of its counterpart.

-- > match_parens "()(()())((()))"
