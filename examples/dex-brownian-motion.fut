-- # Dex: Brownian motion
--
-- The following is a port of
-- [brownian_motion.dx](https://google-research.github.io/dex-lang/brownian_motion.html).
-- It's pretty straightforward.

import "dex-prelude"

type UnitInterval = f64

let bmIter ((key, y, sigma, t): (Key, f64, f64, UnitInterval))
         : (Key, f64, f64, UnitInterval) =
  let (kDraw, kL, kR) = splitKey3 key
  let t' = f64.abs (t - 0.5)
  let y' = sigma * randn kDraw * (0.5 - t')
  let key' = if t > 0.5 then kL else kR
  in (key', y + y', sigma / f64.sqrt 2.0, t' * 2.0)

let sampleBM (key: Key) (t: UnitInterval) : f64 =
  let (_, y, _, _) = iterate 10 bmIter (key, 0.0, 1.0, t)
  in y

let xs = linspace 1000 0.0 1.0
let ys = map (sampleBM (newKey 42)) xs

-- But unfortunately Futhark does not have a simple way to plot those
-- results, so you'll have take our work on this one.
