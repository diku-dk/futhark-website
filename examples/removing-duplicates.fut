-- # Removing duplicates
--
-- The most straightforward way to remove duplicate elements from an
-- array is to sort the array, then use a `filter` to keep only those
-- elements that are either first in the resulting array or differ
-- from the following element.  First we import a sorting function
-- from the [merge sort example](merge-sort.html).

module merge = import "merge-sort"

-- Then we write a function that removes consecutive duplicate
-- elements.

def neq lte x y = if x `lte` y then !(y `lte` x) else true

def pack lte xs =
  zip3 (indices xs) xs (rotate (-1) xs)
  |> filter (\(i,x,y) -> i == 0 || neq lte x y) |> map (.1)

def pack_i32 = pack (i32.<=)

-- > pack_i32 [3,0,0,1,1,1,2,2,3,1]

-- And then by sorting first, a function that removes any duplicate
-- elements.

def dedup_sort lte xs = merge.sort lte xs |> pack lte

def dedup_sort_i32 = dedup_sort (i32.<=)

-- > dedup_sort_i32 [3,0,0,1,1,1,2,2,3,1]

-- One downside of this definition is that the original element order
-- is not preserved, as shown above.  We can address this by
-- associating the original indexes with the input sequence and
-- sorting by those at the end.

def nub lte xs =
  zip xs (indices xs)
  |> merge.sort (\(a,_) (b,_) -> a `lte` b)
  |> pack (\(a,_) (b,_) -> a `lte` b)
  |> merge.sort (\(_,i) (_,j) -> i <= j)
  |> map (.0)

def nub_i32 = nub (i32.<=)

-- > nub_i32 [3,0,0,1,1,1,2,2,3,1]

-- This is a general solution, but all this sorting is quite
-- inefficient.  If we know we will be operating on numbers or
-- number-like data, we can use a [radix sort](radix-sort.html) to
-- make the sorting a bit faster.  However, if we can make assumptions
-- about the input, even more efficient approaches are possible.
--
-- For example, if we are deduplicating `k` numbers in the range
-- `[0,n-1]`, where `k` is much larger than `n`, we can use
-- [scatter](gather-and-scatter.html) to figure out which numbers are
-- contained, followed by a filter:

def dedup_scatter [k] (n: i64) (xs: [k]i64) : []i64 =
  scatter (replicate n false) xs (replicate k true)
  |> zip (iota n)
  |> filter (.1)
  |> map (.0)

--

-- > dedup_scatter 4 [3,0,0,1,1,1,2,2,3,1]

-- Note that this also does not preserve original element order.
--
-- If we can *characterise* the elements to deduplicate by an integer,
-- but they also contain other information we wish to preserve, we
-- need to use a [generalised histogram](histograms.html).  The idea
-- is similar to the `scatter` above, but instead of storing
-- true/false, we store the *lowest index of an element that hits that
-- bucket*, then afterwards each element checks whether it "won".

def nub_hist [k] 't (n: i64) (is: [k]i64) (xs: [k]t) : []t =
  let H = hist i64.min k n is (indices xs)
  in map2 (\i j -> H[i] == j) is (indices xs)
     |> zip xs
     |> filter (.1)
     |> map (.0)

def nub_hist_i32 n is xs : []i32 = nub_hist n is xs

-- > nub_hist_i32 4 [3,0,0,1,1,1,2,2,3,1] [0,1,2,3,4,5,6,7,8,9]

-- This prefers the first occurrence of an element.  It's
-- straightforward to change it to prefer the last occurrence instead.
-- One particularly crucial aspect of this definition is that the
-- operator we use for the histogram, `i64.min`, is likely to be
-- directly supported in hardware, meaning the histogram will be
-- computed very efficiencly.
