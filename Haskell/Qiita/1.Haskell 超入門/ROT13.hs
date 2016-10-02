import Data.Char

rot13 x =
  if n < (65 + 26)
    then chr n
    else chr (((n - 65) `mod` 26) + 65)
  where n = (ord x) + 13

main = do
  print $ map rot13 ['A'..'Z']
