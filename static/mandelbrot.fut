let dot (r: f32, i: f32): f32 =
  r * r + i * i

let multComplex (a: f32, b: f32) (c: f32, d: f32): (f32,f32) =
  (a*c - b * d,
   a*d + b * c)

let addComplex (a: f32, b: f32) (c: f32, d: f32): (f32,f32) =
  (a + c,
   b + d)

let divergence (depth: i32) (c0: (f32,f32)): i32 =
  let (_, i) = loop (c, i) = (c0, 0) while (i < depth) && (dot c < 4.0) do
    (addComplex c0 (multComplex c c),
     i + 1)
  in i

let mandelbrot (screenX: i64) (screenY: i64) (depth: i32)
               (xmin: f32) (ymin: f32) (xmax: f32) (ymax: f32): [screenY][screenX]i32 =
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  in map (\y ->
            map (\x ->
                   let c0 = (xmin + (f32.i64 x * sizex) / f32.i64 screenX,
                             ymin + (f32.i64 y * sizey) / f32.i64 screenY)
                   in (divergence depth c0))
                (iota screenX))
            (iota screenY)

let escapeToColour (depth: i32) (divergence: i32): i32 =
  if depth == divergence
  then 0
  else
    let r = 3 * divergence
    let g = 5 * divergence
    let b = 7 * divergence
    in (r<<16 | g<<8 | b)

let main (screenX: i64) (screenY: i64) (depth: i32)
         (xmin: f32) (ymin: f32) (xmax: f32) (ymax: f32): [screenY][screenX]i32 =
  let escapes = mandelbrot screenX screenY depth xmin ymin xmax ymax
  in map (\row ->
           map (escapeToColour depth) row)
         escapes
