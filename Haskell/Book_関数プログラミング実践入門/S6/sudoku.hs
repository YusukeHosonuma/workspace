import Data.List
import Data.Function

-- | マス（列、行）
type Cell = (Int, Int)

-- | 盤面
type Board = [(Cell, Int)]

-- | 数独のソルバ
solve :: Board -> [Board]
solve board | length board == 81 = [board]
solve board = [ (cell, n) : board
              | let remains = cells \\ map fst board
              , let cell = maximumBy (compare `on` length . used board) remains
              , n <- [1..9] \\ used board cell
              ] >>= solve

-- | 81マス全体
cells :: [Cell]
cells = [ (x, y) | x <- [0..8], y <- [0..8]]

-- | マスの所属する区間
area :: Cell -> Int
area (x, y) = y `div` 3 * 3 + x `div` 3

-- | ある盤面状況で、あるマスの周囲に使われてる数値を列挙する
used :: Board -> Cell -> [Int]
used board cell = nub [ n
                  | (cell', n) <- board
                  , any (\f -> f cell == f cell') [ snd, fst, area ]
                  ]

main :: IO ()
main = case solve problem of
         answer : _ -> mapM_ print $ format answer
         []         -> putStrLn "invalid problem"

-- | 解を軽くフォーマットする
format :: Board -> [[Int]]
format = map (map snd) . transpose . groupBy ((==) `on` (fst . fst)) . sort

-- | 例題
problem :: Board
problem = [ ((3, 0), 8)
          , ((5, 0), 1)
          , ((6, 1), 4)
          , ((7, 1), 3)
          , ((0, 2), 5)
          , ((4, 3), 7)
          , ((6, 3), 8)
          , ((6, 4), 1)
          , ((1, 5), 2)
          , ((4, 5), 3)
          , ((0, 6), 6)
          , ((7, 6), 7)
          , ((8, 6), 5)
          , ((2, 7), 3)
          , ((3, 7), 4)
          , ((3, 8), 2)
          , ((6, 8), 6)
          ]
