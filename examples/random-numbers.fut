-- # Random numbers
--
-- Generating random numbers in a purely functional language requires
-- us to manually track the state of the random number generator
-- (RNG).  The following code shows the idea.  Note that the random
-- numbers we will generate are of mediocre quality at best, but
-- they're good enough for small visualisations and simulations and
-- such.  Random number generation is a very complex subject, but most
-- of the complexities are the same in any language.  Here we focus
-- only on the ones that are particular to Futhark.
--
-- First we define a [module type](abstract-data-types.html)
-- describing the interface of a random number generator.

module type rand = {

-- It consists of a random number generator *state*:

  type rng

-- These states can be initialised from a *seed*:

  val init : i32 -> rng

-- To generate a uniformly distributed random integer, we pass in an
-- RNG state, which will give us back an integer and a new state:

  val rand : rng -> (rng, i32)

-- It is the user's responsibility not to use the same state more than
-- once (unless that is what is desired).
--
-- We will sometimes need to use one seed to generate multiple random
-- streams.  The `split` function splits one RNG state
-- in twain:

  val split : rng -> (rng, rng)

-- And `split_n` splits it arbitrary many ways, which is useful
-- when we wish to compute many parallel streams in parallel:

  val split_n : (n: i64) -> rng -> [n]rng
}

-- We can implement the `rand` module type using various different
-- algorithms.  For this example, we'll go with a simple [linear
-- congruential
-- generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)
-- (LCG):

module lcg : rand = {

-- The RNG state is just 32 bits.

  type rng = u32

-- An LCG is defined by the three constants *a*, *c*, and *m*.  We use
-- the following values, which are the same that are used by glibc:

  def a : u32 = 1103515245
  def c : u32 = 12345
  def m : u32 = 1<<31

-- Note that with these constants, we will never generate a negative
-- random integer.

  def rand rng =
    let rng' = (a * rng + c) % m
    in (rng', i32.u32 rng')

-- Initialisation consists of a few rounds of a [hash function found
-- on
-- StackOverflow](https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key/12996028#12996028),
-- as is tradition.

  def init (x: i32): u32 =
    let x = u32.i32 x
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x)
    in x

-- The main weakness of this initialisation is that a zero seed will
-- result in a zero state, which will be "stuck", producing only
-- zeroes.  So don't do that.
--
-- The splitting functions creates RNG states by slightly twiddling
-- the states.

  def split (rng: rng) =
    (init (i32.u32 (rand rng).0),
     init (i32.u32 rng))

  def split_n n rng =
    tabulate n (\i -> init (i32.u32 rng ^ i32.i64 i))
}

-- This is how we might use it:

def test seed =
  let rng = lcg.init seed
  let (rng, x) = lcg.rand rng
  let (_, y) = lcg.rand rng
  in (x, y)

-- Usually we don't need integers in the full `i32` range, so let's
-- define a handy function for generating integers in the range `[0,
-- bound-1]`:

def rand_i32 (rng: lcg.rng) (bound: i32) =
  let (rng, x) = lcg.rand rng
  in (rng, x % bound)

-- Note that the resulting integers are not fully uniformly
-- distributed unless the span of the RNG is divisible by `bound`.  In
-- practice, as long as `bound` isn't too large, it should not matter
-- much.
--
-- We can also define a function that gives us a random `f64` in the
-- interval `[0,1]`:

def rand_f64 (rng: lcg.rng) =
  let (rng, x) = lcg.rand rng
  in (rng, f64.i32 x / f64.i32 i32.highest)

-- For a real random number package, we'd then go on defining various
-- other function for generating normally distributed numbers.  We
-- would also write these functions as part of a parametric module,
-- rather than hard-coding the use of `lcg`.  All this is done in the
-- [cpprandom](https://github.com/diku-dk/cpprandom) package, which
-- you should use for real Futhark programs.
--
-- # See also
--
-- The [cpprandom](https://github.com/diku-dk/cpprandom) package.
