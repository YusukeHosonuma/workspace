main = do
  let a = return 1 -- return関数でアクションを生成している
  a' <- a
  print a'
  print =<< a
  a >>= print
