main = do
  print $ (\f -> f 1 2) (\x y -> x + y)
