add x = \y -> x + y

main = do
  let add2 = add 2
  print $ add2 3
  print $ (add 2) 3
  print $ add 2 3
