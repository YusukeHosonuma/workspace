f xs g = [g x | x <- xs]

main = do
    print $ f [1..5] (*2)
