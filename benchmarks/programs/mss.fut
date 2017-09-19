-- Parallel maximum segment sum
-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 }

let max(x: i32, y: i32): i32 =
  if x > y then x else y

let redOp(x: (i32,i32,i32,i32))
         (y: (i32,i32,i32,i32)): (i32,i32,i32,i32) =
  let (mssx, misx, mcsx, tsx) = x in
  let (mssy, misy, mcsy, tsy) = y in
  ( max(mssx, max(mssy, mcsx + misy))
  , max(misx, tsx+misy)
  , max(mcsy, mcsx+tsy)
  , tsx + tsy)

let mapOp (x: i32): (i32,i32,i32,i32) =
  ( max(x,0)
  , max(x,0)
  , max(x,0)
  , x)

let main(xs: []i32): i32 =
  let (x, _, _, _) =
    reduce redOp (0,0,0,0) (map mapOp xs) in
  x
