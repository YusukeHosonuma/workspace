main = do
  print $ map (* 2) [1..5]
  print [x * 2 | x <- [1..5]]
