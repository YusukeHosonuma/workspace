fact 0 = return 1
fact n | n > 0 = do
  n' <- fact (n - 1)
  return $ n * n'

main = do
  print =<< fact 5
