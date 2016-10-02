f g = g 2 3

-- add x y = x + y
-- mul = \x y -> x * y

main = do
  print $ f $ \x y -> x + y
  print $ f $ \x y -> x * y
