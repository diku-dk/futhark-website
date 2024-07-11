-- # Line fitting
--
-- The following function uses [linear least
-- squares](https://en.wikipedia.org/wiki/Linear_least_squares) to
-- determine constants `a`, `b` for a function `f(x)=b*x+a` given a
-- collection of observations.

let line_fit [n] (x: [n]f64) (y: [n]f64) =
  let n = f64.i64 n
  let xa = f64.sum x / n
  let ya = f64.sum y / n
  let Stt = f64.sum (map (**2) (map (\x' -> x' - xa) x))
  let b = f64.sum (map2 (*) (map (\x' -> x' - xa) x) y) / Stt
  let a = ya - xa*b
  let chi2 = f64.sum(map2 (\x' y' -> (y'-a-b*x')**2) x y)
  let siga = f64.sqrt((1/n + xa**2/Stt)*chi2/n)
  let sigb = f64.sqrt((1/Stt)*(chi2/n))
  in (a, b, siga, sigb)

-- > line_fit [0.0, 1.0, 2.0, 3.0, 4.0] [2.75, 5.0, 7.0, 9.0, 11.0]
