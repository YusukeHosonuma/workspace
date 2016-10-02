import Data.List

main = do
    content <- getContents
    let ls = lines content
        (minAns, maxAns) = answer ls
    putStrLn $ (show maxAns) ++ " " ++ (show minAns)

answer :: [String] -> (Int, Int)
answer [_, ansLine] =
    let nss = sortByLength . group . sort . map (\c -> read [c] :: Int) $ ansLine
        minAns = if length nss == 1 -- 解答の種類が1つだけなら、最小は0
                    then 0
                    else length . head $ nss
        maxAns = length . last $ nss
    in (minAns, maxAns)
    where
        sortByLength = sortBy (\a b -> length a `compare` length b)
