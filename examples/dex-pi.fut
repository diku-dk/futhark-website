-- # Dex: Monte Carlo estimates of pi
--
-- The following is a port of
-- [pi.dx](https://google-research.github.io/dex-lang/pi.html)

import "dex-prelude"

-- First we import the random number implementation from
-- [random-numbers.fut](random-numbers.html) and define a few wrapper
-- functions.

module random = import "random-numbers"

type Key = random.lcg.rng
let rand = random.rand_f64
let split = random.lcg.split_n
let newKey = random.lcg.init

-- The rest of the definition is pretty much just like how Dex does
-- it.

let estimatePiArea (key:Key) : f64 =
  let (key, x) = rand key
  let (_, y) = rand key
  in 4.0 * f64.bool (sq x + sq y < 1)

let estimatePiAvgVal (key:Key) : f64 =
  let (_, x) = rand key
  in 4.0 * f64.sqrt (1.0 - sq x)

let meanAndStdDev (n: i64) (f: Key -> f64) (key: Key) : (f64, f64) =
  let samps = map f (split n key)
  in (mean samps, std samps)

-- The REPL interpreter is too slow for very many samples, but 10000 is
-- fine:
--
-- ```
-- > meanAndStdDev 10000 estimatePiArea (newKey 42)
-- (3.1444f64, 1.640228228021942f64)
-- > meanAndStdDev 10000 estimatePiAvgVal (newKey 42)
-- (3.142531101443501f64, 0.8906758806365033f64)
-- ```
