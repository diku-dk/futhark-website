-- ---
-- title: "Dex: Prelude"
-- ---
--
-- Translation of various functions from
-- [prelude.dx](https://google-research.github.io/dex-lang/prelude.html).
-- Not all of them, but only what is needed to support the other Dex
-- examples.  We also skip some functions that already exist in
-- Futhark, but under different names - we'll keep writing `f64.i32`
-- instead of `FToI`.

let sq (x: f64) = x * x

let mean [n] (xs: [n]f64) : f64 =
  f64.sum xs / f64.i64 n

let std [n] (xs: [n]f64) =
  f64.sqrt (mean (map sq xs) - sq (mean xs))

let linspace (n: i64) (start: f64) (end: f64) : [n]f64 =
  tabulate n (\i -> start + f64.i64 i * ((end-start)/f64.i64 n))

-- Some Dex programs use this sequential scan.

let scan' n x0 f =
  (.0) <|
  loop (arr, acc) = (replicate n x0, x0) for i < n do
    let acc' = f i acc
    in (arr with [i] = acc', acc')

-- # Random numbers
--
-- The random numbers defined in
-- [random-numbers.fut](random-numbers.html) are based on the idea of
-- having functions take and return random number states.  Dex's
-- approach to random numbers is based on splitting and never
-- returning the final state.  Both work fine in Futhark.  The biggest
-- difference is that the Dex implementation uses a [high-quality hash
-- algorithm](https://github.com/sitmo/threefry), and we use [a hash
-- function found on
-- StackOverflow](https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key/12996028#12996028):

type Key = #Key u32

let hash (k : Key)  (y: i32): Key =
  match k case #Key x ->
    let x = x ^ u32.i32 y
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x)
    in #Key x

let newKey = hash (#Key 0)
let splitKey k = (hash k 1, hash k 2)

let splitKey3 k =
  let (a, k') = splitKey k
  let (b, c) = splitKey k'
  in (a,b,c)

let many '^a (f: Key -> a) (k: Key) (i: i64) = f (hash k (i32.i64 i))

let ixkey (k: Key) (i: i64) : Key = hash k (i32.i64 i)
let ixkey2 (k: Key) (i: i64) (j: i64) : Key =
  hash (hash k (i32.i64 i)) (i32.i64 j)
let rand (k: Key) : f64 =
  match k case #Key x ->
    f64.u32 x / f64.u32 u32.highest
let randVec 'a (n: i64) (f: Key -> a) (k: Key) : [n]a =
  tabulate n (\i -> f (ixkey k i))

let randn (k: Key) : f64 =
  let (k1, k2) = splitKey k
  let u1 = rand k1
  let u2 = rand k2
  in f64.sqrt ((-2.0) * f64.log u1) * f64.cos (2.0 * f64.pi * u2)

let bern (p: f64) (k: Key) = rand k < p

let randnVec (n: i64) (k: Key) : [n]f64 =
  tabulate n (ixkey k >-> randn)

-- The `randIdx` function computes a random index into an array.  In
-- Dex, where indexes are types, this is done by passing in the size
-- of the array as an implicit parameter, and using type inference to
-- determine the right size for any given application.  In Futhark,
-- `randIdx` is just an elaborate way of generating an integer up to
-- an explicitly given bound.

let randIdx (n: i64) (k: Key) =
  let unif = rand k
  in i64.f64 (f64.floor (unif * f64.i64 n))

-- # See also
--
-- [The Futhark
-- prelude](https://futhark-lang.org/docs/prelude/doc/prelude/prelude.html).
