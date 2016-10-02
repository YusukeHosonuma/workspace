f a = if a == 1 then "1" else "?"

main = do
  print $ f 0
  print $ f 1
