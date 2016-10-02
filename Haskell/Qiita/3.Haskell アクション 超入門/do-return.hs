add x y = do
  print x
  print y
  return $ x + y

main = do
  print =<< add 1 2
