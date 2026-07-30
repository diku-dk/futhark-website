-- # Irregular segmented scan
--
-- Accepts an irregular array encoded as a value vector and a flag vector, then
-- performs a segmented scan. A true flag denotes the beginning of a new
-- segment.

def segscan 't [n]
            (op: t -> t -> t)
            (ne: t)
            (flags: [n]bool)
            (vals: [n]t) : [n]t =
  let pairs =
    scan (\(v1, f1) (v2, f2) ->
            let f = f1 || f2
            let v = if f2 then v2 else op v1 v2
            in (v, f))
         (ne, false)
         (zip vals flags)
  let (res, _) = unzip pairs
  in res

def segprefix_sum = segscan (+) 0i32

-- >  segprefix_sum [true, true, false, true, false, false]
--                         [1,2,3,4,5,6]

-- ## See also
--
-- [Segmented reduction](segmented-reduce.html), [segmented
-- iota](segmented-iota.html).
