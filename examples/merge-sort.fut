-- # Merge sort
--
-- A traditional recursive merge sort does not work in Futhark, but we
-- can construct a [bitonic
-- mergesort](https://en.wikipedia.org/wiki/Bitonic_sorter) fairly
-- straightforwardly.  The main limitation is that bitonic sorting
-- requires the input size to be a power of 2, but we accomodate
-- arbitrary sizes by padding with the largest element.
--
-- In most cases using a [radix sort](radix-sort.html) is superior,
-- but merge sorting has the advantage that it only requires an
-- ordering, while radix sort requires that we can decompose the
-- elements into digits.

-- First we need a way to find the largest element of a nonempty array.

def maximum lte xs =
  reduce (\x y -> if x `lte` y then y else x) xs[0] xs

-- Also, we define a function for computing the integral base-2
-- logarithm.  We do this by using the *count leading zeroes*
-- primitive.

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

-- Then we can create a function that pads an array with the largest
-- element, up till the next power of two.

def pad_to k x xs = sized k (xs ++ replicate (k - length xs) x)

def padpow2 lte xs =
  let d = ilog2 (length xs) in
  if d < 0 || length xs == 2**d then (xs, d)
  else (pad_to (2**(d+1)) (maximum lte xs) xs, d+1)

-- Now we can define the bitonic sort itself.  Read the reference
-- above for details on *why* this algorithm works.

def bitonic lte a p q =
  let d = 1 << (p-q) in
  let f i a_i =
    let up1 = ((i >> p) & 2) == 0
    in if (i & d) == 0
       then let a_iord = a[i | d] in
            if (a_iord `lte` a_i) == up1
            then a_iord else a_i
       else let a_ixord = a[i ^ d] in
            if (a_i `lte` a_ixord) == up1
            then a_ixord else a_i
  in map2 f (indices a) a

def sort [n] 't (lte: t -> t -> bool) (xs: [n]t): [n]t =
  let (xs', d) = padpow2 lte xs
  in (loop xs' for i < d do loop xs' for j < i+1 do bitonic lte xs' i j)
     |> take n

-- Note how we preserve only the first `n` elements.  Since we padded
-- with the largest element, this effectively strips off the padding.
--
-- # See also
--
-- [Radix sort](radix-sort.html)
--
-- The [sorts](https://github.com/diku-dk/sorts) library.
