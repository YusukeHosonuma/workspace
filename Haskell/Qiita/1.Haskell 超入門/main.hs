
segments :: [a] -> [[a]]
segments = foldr (++) [] . scanr (\a b -> [a] : map (a:) b) []
