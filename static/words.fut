type char = u8

module words : {
  type word [p]
  val words [n] : [n]char -> ?[p].(word [p] -> ?[m].[m]char, ?[k].[k](word [p]))
} = {

  def segmented_scan 't [n] (g:t->t->t) (ne: t) (flags: [n]bool) (vals: [n]t): [n]t =
    let pairs = scan ( \ (v1,f1) (v2,f2) ->
                         let f = f1 || f2
                         let v = if f2 then v2 else g v1 v2
                         in (v,f) ) (ne,false) (zip vals flags)
    let (res,_) = unzip pairs
    in res

  def is_space (x: char) = x == ' '
  def isnt_space x = !(is_space x)

  def f &&& g = \x -> (f x, g x)

  type word [p] = ([p](), i64, i64)

  def words [n] (s: [n]char) =
    (\(_, i, k) -> #[unsafe] s[i:i+k],
     segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
     |> (id &&& rotate 1)
     |> uncurry zip
     |> zip (indices s)
     |> filter (\(_,(x,y)) -> x > y)
     |> map (\(i,(x,_)) -> ([],i-x+1,x)))
}
