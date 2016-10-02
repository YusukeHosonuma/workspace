fact n
  | n == 0    = 1
  | otherwise = n * fact (n - 1)

main = do
  print $ fact 5
