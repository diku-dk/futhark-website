-- Benchmark with
--
--   futhark bench --backend=opencl higher_order_mandelbrot.fut
--
-- ==
-- entry: bench_mandelbrot bench_numpy_mandelbrot
-- input { 300i64 300i64 }

type complex = (f32, f32)

def dot (r: f32, i: f32): f32 =
  r * r + i * i

def mult_complex (a: f32, b: f32) (c: f32, d: f32): (f32,f32) =
  (a*c - b * d,
   a*d + b * c)

def add_complex (a: f32, b: f32) (c: f32, d: f32): (f32,f32) =
  (a + c,
   b + d)

def divergence (c: complex) (d: i32): i32 =
  let (_, i') =
    loop (z, i) = (c, 0)
    while i < d && dot(z) < 4.0 do
      (add_complex c (mult_complex z z),
       i + 1)
  in i'

def mandelbrot [n][m] (c: [n][m]complex) (d: i32) : [n][m]i32 =
  map (map (\x -> divergence x d)) c

def numpy_mandelbrot [n][m] (c: [n][m]complex) (d: i32) : [n][m]i32 =
  let nm = n*m
  let c' = flatten_to nm c
  let output = replicate nm 0
  let z = replicate nm (0,0)
  let (output, _) =
    loop (output, z) for i < d do
    let notdone = map (\(a,b) -> (a*a + b*b) < 4) z
    let is = map2 (\b i -> if b then i else -1) notdone (iota nm)
    let inc = map2 add_complex (map (\x -> mult_complex x x) z) c'
    in (scatter output is (replicate nm i),
        scatter z is inc)
  in unflatten n m output

def complexes (screenX: i64) (screenY: i64)
              (xmin: f32) (ymin: f32) (xmax: f32) (ymax: f32) =
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  in map (\y ->
            map (\x ->
                   (xmin + (f32.i64 x * sizex) / f32.i64 screenX,
                    ymin + (f32.i64 y * sizey) / f32.i64 screenY) )
                (iota screenX))
            (iota screenY)

entry bench_mandelbrot w h = mandelbrot (complexes w h (-1) (-1) 1 1) 255

entry bench_numpy_mandelbrot w h = numpy_mandelbrot (complexes w h (-1) (-1) 1 1) 255
