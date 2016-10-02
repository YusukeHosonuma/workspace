-- | 最大値を求める
maximum' :: (Ord a) => [a] -> a
maximum' []     = error "List is empty."
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | (a == x)  = True
  | otherwise = elem' a xs

qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = (qsort lt) ++ [x] ++ (qsort gt)
  where
    lt = filter (<= x) xs
    gt = filter (>  x) xs
