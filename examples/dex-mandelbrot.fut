-- # Dex: Mandelbrot set
--
-- The following is a port of
-- [mandelbrot.dx](https://google-research.github.io/dex-lang/mandelbrot.html).
--
-- The escape time algorithm needs complex numbers, so we import the
-- definition from the [complex number example](complex-numbers.html).

module complex = import "complex-numbers"
module c64 = complex.c64
type c64 = c64.complex

let update (c: c64) (z: c64) = c64.(c + z * z)

let tol : f64 = 2.0

let inBounds (z: c64) = c64.mag z < tol

-- In Dex, type trickery (the `Fin 1000` type) is used to implicitly
-- fold across an imaginary 1000-element array.  In Futhark, we use an
-- old-fashioned `for`-loop.

let escapeTime (c: c64) =
  (loop (n, z) = (0, c64.mk 0 0) for _i < 1000 do
   let z' = update c z
   in (n + f64.bool (inBounds z'), z')).0

-- A more idiomatic Futhark implementation would use a `while`-loop
-- ([as here](../static/mandelbrot.fut)).
--
-- Futhark does not directly have anything like Dex's built-in
-- plotting support, but we can write a simple `main` function that
-- produces a two-dimensional array of floats.

let main h w =
  tabulate_2d h w
              (\j i ->
                 let x = -2.0 + f64.i64 i * (3/f64.i64 w)
                 let y =   -1 + f64.i64 j * (2/f64.i64 h)
                 in 1-escapeTime (c64.mk x y) / 1000)

-- We can then compile the program and use the
-- [data2png.py](https://github.com/diku-dk/futhark/blob/master/tools/data2png.py)
-- script to convert its output to an image:
--
-- ```
-- $ futhark c dex-mandelbrot.fut
-- $ echo 200 300 | ./dex-mandelbrot -b | data2png.py /dev/stdin dex-mandelbrot.png
-- ```
--
-- ![](dex-mandelbrot.png)
