import Data.Char

rot13ch ch
  |  'A' <= ch && ch <= 'M'
  || 'a' <= ch && ch <= 'm' = chr $ ord ch + 13
  |  'N' <= ch && ch <= 'Z'
  || 'n' <= ch && ch <= 'z' = chr $ ord ch - 13
  | otherwise = ch

rot13 = map rot13ch

main = do
  let hello13 = rot13 "Hello, World!"
  print hello13
  print $ rot13 hello13
