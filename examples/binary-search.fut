-- # Binary search
--
-- When we need to look for the position of an element in a sorted
-- sequence, we can use a [binary
-- search](https://en.wikipedia.org/wiki/Binary_search_algorithm) to
-- perform the lookup in *O(log(n))* steps instead of the *O(n)* that
-- would normally be required.  The difference is often large enough
-- that even if the sequence isn't sorted in advance, it may be worth
-- sorting it if we need to do many lookups.  Since Futhark is a
-- parallel language, this is the case we care about.
--
-- In Futhark it is pretty straightforward to implement a polymorphic
-- function that given a less-than-or-equal comparison operator
-- performs a binary search for an element in an (assumed) sorted
-- array:

let binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n-1) while l < r do
    let t = l + (r - l) / 2
    in if x `lte` xs[t]
       then (l, t)
       else (t+1, r)
  in l

-- This function returns the index where the `x` element would occur
-- *if it occurs at all*.  Examples:
--
-- ```
-- > binary_search (<=) [1,3,5,7,9,11] 0
-- 0i64
-- > binary_search (<=) [1,3,5,7,9,11] 9
-- 4i64
-- > binary_search (<=) [1,3,5,7,9,11] 2
-- 1i64
-- ```
--
-- Note that when we look for `2`, we return the index `1`, which
-- contains the value `3`.  Callers must look up the returned index
-- and check whether it is actually pointing to the element you were
-- looking for.  That's perhaps a bit unwieldy, and we could use
-- [Futhark's sum types](sum-types.html) to make this a bit nicer.
--
-- While the binary search shown above is *asymptotically* optimal, it
-- is known to be inefficient in practice, because it jumps all over
-- memory, yielding very poor [cache
-- locality](https://en.wikipedia.org/wiki/Locality_of_reference).
-- [This tutorial](https://algorithmica.org/en/eytzinger) (based on
-- [this paper](https://arxiv.org/pdf/1509.05053.pdf)) shows that if
-- we lay out the array in
-- [BFS](https://en.wikipedia.org/wiki/Breadth-first_search)-order,
-- then the levels of the search tree are stored consecutively in
-- memory, yielding much better locality.  The tutorial also goes into
-- detail about various other low-level optimisations, but we will
-- focus on the alternative data layout, in particular whether it is
-- useful when generating GPU code.
--
-- While GPUs have caches, they are not usually as crucial as when
-- doing CPU programming.  What is more important is [memory
-- coalescing](https://cvw.cac.cornell.edu/GPU/coalesced), which is
-- roughly about ensuring that neighbouring threads access
-- neighbouring memory addresses in the same clock cycle, as this
-- allows full utilisation of the very wide memory bus of the GPU.  We
-- probably can't get coalescing when executing something as
-- unstructured as a binary search, but maybe using an Eytzinger
-- representation can help a bit.
--
-- First we ned to implement a function for constructing an Eytzinger
-- array (which models a search tree) from a sorted array.  The
-- tutorial shows an elegant, but sequential, C function for doing
-- this.  No good for Futhark.  Fortunately, for input sizes that are
-- one less than a power of two (so they fill out an entire binary
-- tree), there is a closed formula that given an index *i* in an
-- *n*-element Eytzinger array produces the index of the corresponding
-- element in the original sorted array.  It is based on computing the
-- following derived quantities (don't worry about the details if you
-- find them fiddly - they *are* fiddly):
--
-- * *lvl*: the number of edges from the root to this element.
--   Equivalently, the number of full levels of the search tree that
--   precede this element.  This can be computed as the binary
--   logarithm of *i+1* (rounded up).
--
-- * *offset*: the number of elements in the sorted array that
--   separate each element of this level in the Eytzinger tree.  This
--   quantity halves for every level in the tree.
--
-- * *k*: index of the last element of the level above the one
--   containing *i*.  This means that *i-k* is index of *i* within the
--   current level of the tree.
--
-- We can implement this in Futhark as follows.  First we define an
-- efficient integer binary logarithm function by using the *count
-- leading zeroes* primitive function:

let log2 x = 63 - i64.clz x

-- Then we can define `eytzinger_index` itself:

let eytzinger_index (n: i64) (i: i64) =
  let lvl = log2 (i+1)
  let offset = i64.i32 (1<<(log2 n-lvl))
  let k = i64.i32 ((1<<lvl)-1)
  in offset + (i-k) * offset * 2 - 1

-- Finally, the `eytzinger` function applies `eytzinger_index` on
-- every integer from *0* to *n-1*, and reads the corresponding
-- element from the sorted array `xs`.

let eytzinger [n] 't (xs: [n]t) : [n]t =
  let f i = xs[eytzinger_index n i]
  in tabulate n f

-- Constructing the Eytzinger array is the only really fiddly part of
-- this.  The search function can be ported fairly directly from the C
-- function,although we do have to implement our own [find first
-- bit](https://man7.org/linux/man-pages/man3/ffs.3.html) function,
-- since it's not a Futhark primitive:

let ffs x = i64.ctz x + 1

-- Now we can define `eytzinger_search`, where we assume `xs` is an
-- Eytzinger array:

let eytzinger_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let k =
    loop k = 1 while k <= n do
      if x `lte` xs[k-1]
      then 2*k
      else 2*k+1
  in (k >> i64.i32 (ffs (!k)))-1

-- Alright, let's benchmark this.  Unfortunately, while Futhark's
-- [benchmarking
-- tool](https://futhark.readthedocs.io/en/stable/man/futhark-bench.html)
-- supports randomly generated input of any desired size, it does
-- *not* provide any way of generating *sorted* input, which we ned
-- here.  We thus have to generate the input manually.  First, we need
-- a function for sorting.  We should of course use a [proper sorting
-- library](https://github.com/diku-dk/sorts), but it's awfully
-- tempting to just use the [radix sort example](radix-sort.html) that
-- we already have in this very same subdirectory...

module sort = import "radix-sort"

entry sorted (xs: []i32) =
  xs |> map u32.i32 |> sort.radix_sort |> map i32.u32

-- The only quirk is that our radix sort operates on unsigned 32-bit
-- integers, so we need to convert back and forth.  This is fine as
-- long as we are careful not to make our input too large.  Now we can
-- compile the program:
---
-- ```
-- $ futhark opencl binary-search.fut
-- ```
--
--
-- Then we generate *2²⁵-1* with [futhark
-- dataset](https://futhark.readthedocs.io/en/stable/man/futhark-dataset.html),
-- pass it to the `sorted` function, and dump the resulting array to
-- the file `input`.
--
-- ```
-- $ futhark dataset -b --i32-bounds=0:33554430 -g '[33554431]i32' | ./binary-search -b -e sorted >input
-- ```

-- I am rather curious about how long it takes to compute the
-- Eytzinger array, so we define an entry point that calls
-- `eytzinger`, and also provide an input block that will be picked up
-- by `futhark bench`:

-- ==
-- entry: to_eytzinger
-- compiled input @ input
entry to_eytzinger : []i32 -> []i32 = eytzinger

-- But more importantly, we use the `to_eytzinger` entry point to
-- generate a file containing the Eytzinger array:
--
-- ```
-- $ ./binary-search -b -e to_eytzinger <input >input_eytzinger
-- ```

-- Then we define two entry points that search using either the naive
-- strategy, or the Eytzinger array.  In both cases we also use the
-- input array as the keys to search for.  I don't think this skews
-- the results, and it ensures we search for every element in the
-- array.

-- ==
-- entry: bench_binary_search
-- compiled input @ input
entry bench_binary_search [n] (xs: [n]i32) =
  map (binary_search (<=) xs) xs

-- ==
-- entry: bench_eytzinger_search
-- compiled input @ input_eytzinger
entry bench_eytzinger_search [n] (xs: [n]i32) =
  map (eytzinger_search (<=) xs) xs

-- Finally, as a point of comparison, we also want to measure how long
-- it would take to just copy the input:

-- ==
-- entry: bench_copy
-- compiled input @ input
entry bench_copy [n] (xs: [n]i32) =
  copy xs

-- Now we are ready for benchmarking, with 100 runs per entry point:
--
-- ```
-- $ futhark bench binary-search.fut --backend=opencl -r 100
-- ```
--
-- These results are from an NVIDIA RTX 2080 Ti GPU:
--
-- ```
-- Results for binary-search.fut:to_eytzinger:
-- input:                 1698μs (RSD: 0.009; min:  -2%; max:  +3%)
--
-- Results for binary-search.fut:bench_binary_search:
-- input:                 2670μs (RSD: 0.003; min:  -1%; max:  +1%)
--
-- Results for binary-search.fut:bench_eytzinger_search:
-- input_eytzinger:       2107μs (RSD: 0.009; min:  -2%; max:  +4%)
--
-- Results for binary-search.fut:bench_copy:
-- input:                  534μs (RSD: 0.006; min:  -1%; max:  +4%)
-- ```
--
-- The Eytzinger approach is only 20% faster than the naive approach,
-- so there is not much advantage.  However, it only takes three times
-- as long to compute the Eytzinger array as it takes to copy the
-- input array, so if we want to do an enormous amount of lookups, it
-- may be profitable.  It's not as profitable as on a Xeon E5-2650
-- CPU, though:
--
-- ```
-- $ futhark bench binary-search.fut --backend=c -r 10
-- Compiling binary-search.fut...
-- Reporting average runtime of 10 runs for each dataset.

-- Results for binary-search.fut:to_eytzinger:
-- input:               417177μs (RSD: 0.003; min:  -0%; max:  +0%)
--
-- Results for binary-search.fut:bench_binary_search:
-- input:              4293823μs (RSD: 0.021; min:  -2%; max:  +4%)
--
-- Results for binary-search.fut:bench_eytzinger_search:
-- input_eytzinger:    2749538μs (RSD: 0.025; min:  -2%; max:  +6%)
--
-- Results for binary-search.fut:bench_copy:
-- input:                56820μs (RSD: 0.008; min:  -1%; max:  +2%)
-- ```
