doubleMe  x = x + x
doubleMe' x = x * 2

isZero x = if x == 0
             then "Yes"
             else "No"

lucky :: Int -> String
lucky 7 = "Lucky!"
lucky x = "Unlucky..."

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fruits :: Char -> String
fruits 'A' = "Apple"
fruits 'B' = "Banana"

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' []     = error "List is empty."
head' (x:xs) = x

tail' :: [a] -> [a]
tail' []     = error "List is empty."
tail' (x:xs) = xs

head2 :: [a] -> [a]
head2 []       = error "List is empty."
head2 (x:y:ys) = [x, y]

firstLetter :: String -> String
firstLetter s@(x:xs) = s ++ "'s first letter is " ++ [x]

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b    = EQ
  | a <= b    = LT
  | otherwise = GT

-- | BMI判定（体重kg、身長cm）
judgeBMI :: Double -> Double -> String
judgeBMI weight height
  | bmi < 18.5 = "低体重" ++ show bmi
  | bmi < 25.0 = "標準"
  | bmi < 30.0 = "肥満度1"
  | bmi < 35.0 = "肥満度2"
  | bmi < 40.0 = "肥満度3"
  | otherwise  = "肥満度4"
  where
    bmi     = weight / (heightM * heightM)
    heightM = height / 100

hello :: String -> String
hello name = "Hello, " ++ mr name
  where
    mr name = "Mr." ++ name

-- | イニシャルを取得
initials :: String -> String -> String
initials firstname lastname = [l] ++ "." ++ [f]
  where
    (f:_) = firstname
    (l:_) = lastname

-- | 複数のBMIを計算
bmis :: [(Double, Double)] -> [Double]
bmis xs = [ bmi | (w, h) <- xs, let hm  = h / 100
                                    bmi = w / (hm * hm) ]

sayNumber :: Int -> String
sayNumber n = (show n) ++ " is "
              ++ case n of
                1 -> "One"
                2 -> "Two"
                3 -> "Three"
                _ -> "Number!"


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- | 再帰バージョン
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- | foldr版
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> (f x) : acc) []

-- | 再帰バージョン
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : (filter' f xs)
  | otherwise = filter' f xs

-- | foldr版
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

reduce' :: (a -> b -> a) -> a -> [b] -> a
reduce' _ a [] = a
reduce' f a (x:xs) = reduce' f (f a x) xs

collatz :: Int -> [Int]
collatz x
  | x == 1 = [1]
  | even x = x : (collatz $ x `div` 2)
  | odd  x = x : (collatz $ x * 3 + 1)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: [Int] -> Int
product' = foldr (*) 1

last' :: [a] -> a
last' = foldl1 (\acc x -> x)

-- | すごいHaskell楽しく学ぼう :: 関数合成版
-- replicate 2 . product . map (*3) $ zipWith max [1, 2] [4, 5]
--
-- | オリジナルパイプ版（UNIXライク）
-- zipWith max [1, 2] [4, 5] ＞ (map (*3) ｜ product ｜ replicate 2)
--
(｜) :: (a -> b) -> (b -> c) -> (a -> c)
g ｜ f = \x -> f $ g x

(＞) :: a -> (a -> b) -> b
x ＞ f = f x
