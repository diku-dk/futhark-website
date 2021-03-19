## One-dimensional tiling

```Futhark
tabulate n (\i ->
  loop acc = ... for x in xs do
    ...)
```

```Futhark
tabulate (n/B) (\b ->
  tabulate (n%B) (\t ->
    let i = b * B + t
    in loop acc = ...
       for x in xs do
         ...))
```

```Futhark
tabulate (n/B) (\b ->
  tabulate (n%B) (\t ->
    loop acc = ... for l < k / B do
      let chunk = xs[l*B:(l+1)*B]
      let i = b * B + t
      in loop acc = acc
         for x in chunk do
           ...))
```

```Futhark
tabulate (n/B) (\b ->
  loop accs = ... for l < k / B do
    let chunk = copy xs[l*B:(l+1)*B]
    in tabulate (n%B) (\t ->
         let i = b * B + t
         in loop acc = accs[i]
            for x in chunk do
              ...))
```

## Two-dimensional block tiling

```Futhark
tabulate_2d n (\i j ->
  loop acc = ... for (x,y) in zip xs[i,:] ys[j,:] do
    ...)
```

```Futhark
tabulate_2d (n/B) (m/B) (\b_i b_j ->
  tabulate_2d (n%B) (m%B) (\t_i t_j ->
    let i = b_i * B + t_i
    let j = b_j * B + t_j
    in loop acc = ...
       for (x,y) in zip xs[i,:] ys[j,:] do
         ...))
```

```Futhark
tabulate_2d (n/B) (m/B) (\b_i b_j ->
  tabulate_2d (n%B) (m%B) (\t_i t_j ->
    loop acc = ... for l < k / B do
      let xs_chunk = xs[b_i*B:(b_i+1)*B, l*B:(l+1)*B]
      let ys_chunk = ys[l*B:(l+1)*B,     b_j*B:(b_j+1)*B]
      in loop acc = acc
         for (x,y) in zip xs[t_i,:] ys_chunk[t_j,:] do
           ...))
```

```Futhark
tabulate_2d (n/B) (m/B) (\b_i b_j ->
  loop accs = ... for l < k / B do
    let xs_chunk = copy xs[b_i*B:(b_i+1)*B, l*B:(l+1)*B]
    let ys_chunk = copy ys[l*B:(l+1)*B,     b_j*B:(b_j+1)*B]
    in tabulate_2d (n%B) (m%B) (\t_i t_j ->
         loop acc = accs[t_i,t_j]
         for (x,y) in zip xs_chunk[t_i,:] ys_chunk[t_j,:] do
           ...))
```

## Two-dimensional block and register tiling

```Futhark
tabulate_2d n (\i j ->
  loop acc = ... for (x,y) in zip xs[i,:] ys[j,:] do
    ...)
```

```Futhark
tabulate_2d (n/(B*R)) (m/(B*R)) (\b_i b_j ->
  tabulate_2d (n%B) (m%B) (\t_i t_j ->
    let i = b_i * B + t_i
    let j = b_j * B + t_j
    in loop acc = ...
       for (x,y) in zip xs[i,:] ys[j,:] do
         ...))
```
