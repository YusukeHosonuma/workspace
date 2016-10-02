
-- 最大値を一番上に
bsort1 [x] = [x]
bsort1 (x:y:ys)
  | x < y = x:y:ys
  | otherwise = y : bsort1 (x:ys)

-- バブルソート
bsort [] = []
bsort (x:xs) = bsort1 $ x:(bsort (xs))

-- bsort [7, 2] -> bsort1 [7, 2] -> [2, 7]
-- bsort [1, 7, 2] -> bsort1 1 bsort

main = do
  print $ bsort [4, 6, 9, 8, 3, 5, 1, 7, 2]
