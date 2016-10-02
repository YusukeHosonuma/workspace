inc x = return $ x + 1

main = do
  return 2 >>= inc >>= print
  print =<< inc =<< return 2
