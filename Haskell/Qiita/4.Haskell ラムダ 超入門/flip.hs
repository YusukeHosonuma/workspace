src = [1..5]
test1 = flip map src
test2 = (`map` src)
test3 f = map f src

main = do
  print $ test1 (* 2)
  print $ test2 (* 2)
  print $ test3 (* 2)
