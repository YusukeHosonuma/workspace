f1 g = g 1
f2 g = g 2 3

main = do
    print $ f1 $ (+ (-3)) -- \x -> x - 3
    print $ f1 $ (3 -)    -- \x -> 3 - x
    print $ f2 $ (+)      -- \x y -> x + y
