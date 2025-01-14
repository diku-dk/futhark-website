-- # Swapping two elements of an array
--
-- You can swap two elements of an array as follows:

def swap 't (i: i64) (j: i64) (A: *[]t) =
  let tmp = copy A[j]
  let A[j] = copy A[i]
  let A[i] = tmp
  in A

-- This uses [in-place updates](inplace.html), which interacts with
-- the [uniqueness type
-- system](https://futhark.readthedocs.io/en/latest/language-reference.html#in-place-updates), which is why the `A` parameter has the `*` annotation.
--
-- ## See also
--
-- [Uniqueness Types and In-Place
-- Updates](https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html)
