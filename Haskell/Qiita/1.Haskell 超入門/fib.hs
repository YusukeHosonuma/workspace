fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = do
  print $ map fib [1..10]
