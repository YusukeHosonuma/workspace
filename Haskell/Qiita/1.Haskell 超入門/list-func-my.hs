sum' [] = 0
sum' (x:xs) = x + (sum' xs)

product' [] = 1 -- 1 * (2 * (3 * (4 * (5 * 1))))
product' (x:xs) = x * (product' xs)

take' _ [] = []
take' 0 _  = []
take' n (x:xs) = x : take' (n - 1) xs-- take 2 [1,2,3,4] ->  1 : [2, 3, 4]

drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs -- drop 2 [1,2,3] -> * [2, 3] -> * * [3]

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x] -- reverse [1,2,3,4] -> reverse [2,3,4] :1

fact 0 = 1
fact n | n > 0 = product [1..n]

main = do
  print $ length [1, 2, 3]
  print $ sum' [1..5]
  print $ product [1..5]
  print $ take 2 [1, 2, 3]
  print $ drop 2 [1, 2, 3]
  print $ reverse [1..5]
  print $ map fact [1..10]
