main = do
  print $ filter (< 5) [1..9]
  print [x | x <- [1..9], x < 5]
