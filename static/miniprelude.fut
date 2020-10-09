let zip [n] 'a 'b (as: [n]a) (bs: [n]b): [n](a,b) =
  map (\i -> (as[i], bs[i])) (iota n)

let iota (n: i64) =
  0..1..<n

let replicate 'a (n: i64) (x: a) =
  map (\_ -> x) (iota n)

let concat 't (xs: []t) (ys: []t): *[]t =
  map (\i -> if i < length xs
             then xs[i]
             else ys[i - length xs])
      (iota (length xs + length ys))

let rotate 't (r: i64) (xs: []t) =
  map (\i -> xs[(i+r) % length xs])
      (iota (length xs))

let transpose [n] [m] 't (a: [n][m]t): [m][n]t =
  map (\i -> map (\j -> a[j,i]) (iota n)) (iota m)

let flatten [n][m] 't (xs: [n][m]t): []t =
  map (\i -> xs[i/m, i%m]) (iota (n*m))

let unflatten 't (n: i64) (m: i64) (xs: []t): [n][m]t =
  map (\i -> map (\j -> xs[i*m+j]) (iota m)) (iota n)

let div_rounding_up (x: i64) (y: i64) =
  (x + y - 1) / y

let reduce_tree 'a (op: a -> a -> a) (ne: a) (as: []a): a =
  let as' = loop as while length as > 1 do
              map (\i ->
                     let x = if i*2 >= length as
                             then ne
                             else as[i*2]
                     let y = if i*2+1 >= length as
                             then ne
                             else as[i*2+1]
                     in x `op` y)
                  (iota (length as `div_rounding_up` 2))
  in if length as' == 0 then ne else as'[0]

let num_threads : i64 = 128 * 256

let reduce [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a): a =
  let chunk_size = n `div_rounding_up` num_threads
  let partial_results =
    map (\t -> loop x = ne for i < chunk_size do
               let j = t + i * num_threads
               in if j < n then x `op` as[j]
                  else x)
        (iota num_threads)
  in reduce_tree op ne partial_results

let scan [n] 'a (op: a -> a -> a) (_ne: a) (as: [n]a): [n]a =
  let iters = i64.f32 (f32.ceil (f32.log2 (f32.i64 n)))
  in loop as for i < iters do
       map (\j -> if j < 2**i
                  then as[j]
                  else as[j] `op` as[j-2**i])
           (iota n)

let filter 'a (p: a -> bool) (as: []a): *[]a =
  let keep = map (\a -> if p a then 1 else 0) as
  let offsets = scan (+) 0 keep
  let num_to_keep = reduce (+) 0 keep
  in if num_to_keep == 0
     then []
     else scatter (replicate num_to_keep as[0])
                  (map (\(i, k) -> if k == 1 then i-1 else -1)
                       (zip offsets keep))
                  as

let stream_map 'a 'b (f: (c: i64) -> [c]a -> [c]b) (as: []a): []b =
  as |> unflatten (length as) 1 |> map (f 1) |> flatten

let stream_red 'a 'b (op: b -> b -> b) (f: (c: i64) -> [c]a -> b) (as: []a): b =
  as |> unflatten (length as) 1 |> map (f 1) |> reduce op (f 0 [])
