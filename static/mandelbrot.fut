-- Make all decimal literals of type f32.
default(f32)

fun dot(c: (f32,f32)): f32 =
  let (r, i) = c
  in r * r + i * i

fun multComplex(x: (f32,f32), y: (f32,f32)): (f32,f32) =
  let (a, b) = x
  let (c, d) = y
  in (a*c - b * d,
      a*d + b * c)

fun addComplex(x: (f32,f32), y: (f32,f32)): (f32,f32) =
  let (a, b) = x
  let (c, d) = y
  in (a + c,
      b + d)

fun divergence(depth: int, c0: (f32,f32)): int =
  loop ((c, i) = (c0, 0)) = while i < depth && dot(c) < 4.0 do
    (addComplex(c0, multComplex(c, c)),
     i + 1)
  in i

fun mandelbrot(screenX: int, screenY: int, depth: int,
               xmin: f32, ymin: f32, xmax: f32, ymax: f32): [screenY][screenX]int =
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  in map (\(y: int): [screenX]int ->
            map (\(x: int): int ->
                   let c0 = (xmin + (f32(x) * sizex) / f32(screenX),
                             ymin + (f32(y) * sizey) / f32(screenY))
                   in divergence(depth, c0))
                (iota screenX))
            (iota screenY)

fun escapeToColour(depth: int) (divergence: int): int =
  if depth == divergence
  then 0
  else
    let r = 3 * divergence
    let g = 5 * divergence
    let b = 7 * divergence
    in (r<<16 | g<<8 | b)

fun main(screenX: int, screenY: int, depth: int,
         xmin: f32, ymin: f32, xmax: f32, ymax: f32): [screenY][screenX]int =
  let escapes = mandelbrot(screenX, screenY, depth, xmin, ymin, xmax, ymax)
  in map(\(row: []int): [screenX]int ->
           map (escapeToColour depth) row)
         escapes
