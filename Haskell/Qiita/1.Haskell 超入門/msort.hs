import Debug.Trace

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort [x] = [x]
msort xs = trace (show ret) ret
  where
    ret = merge (msort b1) (msort b2)
    b1 = take p xs
    b2 = drop p xs
    p  = (length xs) `div` 2

main = do
  print $ msort [4, 6, 9, 8, 3, 5, 1, 7, 2]
