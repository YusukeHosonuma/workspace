add x y = return $ x + y

main = do
  print =<< add 1 2
