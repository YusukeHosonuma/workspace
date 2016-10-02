fib n = case n of
  0 -> 0
  1 -> 1
  _ | n > 0 -> fib (n - 2) + fib (n - 1)

main = do
  print $ map fib [1..10]
