main = do
  print $ "abcde"
  print $ ['a', 'b', 'c', 'd', 'e']
  print $ ['a'..'e']
  print $ 'a':"bcde"
  print $ 'a':'b':"cde"
  print $ "abc" ++ "de"
  print $ "abcde" !! 3
