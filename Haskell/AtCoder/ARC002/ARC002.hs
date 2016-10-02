-- ARC002.hs
import Control.Monad
import Data.Text (split, pack, unpack)

main = do
    contents <- getContents
    let [start, end] = map unpack $ split (== ' ') $ pack contents
        ans = answer (read start :: Int) (read end :: Int)
    putStrLn $ show ans

answer :: Int -> Int -> Int
answer start end
    | start == end = 0
    | otherwise    = (+1) . length . takeWhile isNotIn . map (anyChange start) $ [1..]
        where isNotIn = not . (end `elem`)

anyChange :: Int -> Int -> [Int]
anyChange start x = return start >>= foldr (<=<)
    return (replicate x change)

change :: Int -> [Int]
change n = do
    [n + 1, n - 1, n + 5, n - 5, n + 10, n - 10]
