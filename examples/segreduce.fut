-- # Irregular segmented reduction
--
-- Accepts an irregular array encoded as a value vector and a flag vector, then
-- performs a segmented reduction. The idea is to do a [segmented
-- scan](segmented-scan.html), then extract the last element of every segment.

import "segscan"

def segreduce 't [n]
              (op: t -> t -> t)
              (ne: t)
              (flags: [n]bool)
              (vals: [n]t) : ?[m].[m]t =
  segscan op ne flags vals
  |> zip (rotate 1 flags)
  |> filter (.0)
  |> map (.1)

def segsum = segreduce (+) 0i32

-- >  segsum [true, true, false, true, false, false]
--           [1,2,3,4,5,6]
