fact 0 = do
  putStrLn "fact 0 = 1"
  return 1

fact n | n > 0 = do
  let dbg = "fact " ++ show n ++ " = " ++
            show n ++ " * fact " ++ show (n - 1)
  putStrLn dbg
  n' <- fact (n - 1)
  let ret = n * n'
  putStrLn $ dbg ++ " = " ++ show n ++ " * " ++ show n' ++ " = " ++ show ret
  return ret

main = do
  print =<< fact 5
