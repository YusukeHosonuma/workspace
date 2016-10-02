first (x:_) = x
second (_:x:_) = x

main = do
  print $ first [1..5]
  print $ first "abcdef"
  print $ second [1..5]
  print $ second "abcdef"
