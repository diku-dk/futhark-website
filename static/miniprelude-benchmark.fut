module miniprelude = import "miniprelude"

-- ==
-- entry: bench_sum bench_sum_mini_tree bench_sum_mini
-- compiled random input { [1000]i32 } output { -1712592788i32 }
-- compiled random input { [10000]i32 } output { -1752694635i32 }
-- compiled random input { [100000]i32 } output { 948113170i32 }
-- compiled random input { [1000000]i32 } output { -1902114325i32 }
-- compiled random input { [10000000]i32 } output { -1742265527i32 }
-- compiled random input { [100000000]i32 } output { 655409313i32 }

entry bench_sum = reduce (+) 0i32
entry bench_sum_mini_tree xs = unsafe miniprelude.reduce_tree (+) 0i32 xs
entry bench_sum_mini xs = unsafe miniprelude.reduce (+) 0i32 xs

-- ==
-- entry: bench_prefix_sum bench_prefix_sum_mini
-- compiled random input { [1000]i32 }
-- compiled random input { [10000]i32 }
-- compiled random input { [100000]i32 }
-- compiled random input { [1000000]i32 }
-- compiled random input { [10000000]i32 }
-- compiled random input { [100000000]i32 }

entry bench_prefix_sum = scan (+) 0i32
entry bench_prefix_sum_mini xs = unsafe miniprelude.scan (+) 0i32 xs

-- ==
-- entry: bench_dotprod bench_dotprod_mini
-- compiled random input { [1000]i32 [1000]i32 }
-- compiled random input { [10000]i32 [10000]i32 }
-- compiled random input { [100000]i32 [100000]i32 }
-- compiled random input { [1000000]i32 [1000000]i32 }
-- compiled random input { [10000000]i32 [10000000]i32 }
-- compiled random input { [100000000]i32 [100000000]i32 }

entry bench_dotprod [n] (xs: [n]i32) (ys: [n]i32): i32 =
  reduce (+) 0 (map (\(x, y) -> x*y) (zip xs ys))

entry bench_dotprod_mini [n] (xs: [n]i32) (ys: [n]i32): i32 =
  unsafe miniprelude.(reduce (+) 0 (map (\(x, y) -> x*y) (zip xs ys)))
