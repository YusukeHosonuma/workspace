fact 0 = 1
fact n | n > 0 = n * fact (n - 1)

main = do
  print $ fact (-1)
