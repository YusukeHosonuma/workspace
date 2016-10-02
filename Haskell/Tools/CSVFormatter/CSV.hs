-- CSVFormatter.hs
module CSV
( CSVFormatter
, parseCSV
, formatCSVLines
, formatCSVMarkdown
) where

import Data.Char
import Data.Text (split, pack, unpack)
import Data.List

-- | CSV型
type CSV = [[String]]

-- | CSVフォーマット型
type CSVFormatter = CSV -> [String]

-- | CSVをパース
--
-- Input:
--
-- Name,Birthday,Height
-- Suzukaze Aoba,2/2,149cm
-- Ko Yagami,8/2,164cm
-- Rin Toyama,12/3,158cm
--
parseCSV :: Char -> String -> CSV
parseCSV delimiter = map (map unpack . split (== delimiter) . pack) . lines

-- | CSVをフォーマット
--
-- Output:
--
-- Name            | Birthday | Height
-- -----------------------------------
-- Suzukaze Aoba   | 2/2      | 149cm
-- Ko Yagami       | 8/2      | 164cm
-- Rin Toyama      | 12/3     | 158cm
--
formatCSVLines :: CSVFormatter
formatCSVLines csv = insertSeparator csvLines
    where csvLines = map (intercalate " | ") $ spaced
          spaced = map (\xs -> zipWith padRight xs $ maxLengths csv) csv

-- | CSVをMarkdownのテーブル形式でフォーマット
--
-- | Header One     | Header Two     |
-- | :------------- | :------------- |
-- | Item One       | Item Two       |
--
formatCSVMarkdown :: CSVFormatter
formatCSVMarkdown csv = csvLines
    where csvLines  = map (\line -> "| " ++ intercalate " | " line ++ " |") $ joined
          joined    = head spaced : separator : tail spaced
          spaced    = map (\xs -> zipWith padRight xs $ maxLengths csv) csv
          separator = map (\n -> ':' : replicate (n - 1) '-') $ maxLengths csv

-- | 1行目と2行目の間にセパレータ（----）を入れる
insertSeparator :: [String] -> [String]
insertSeparator xs = head xs : replicate (stringWidth $ head xs) '-' : tail xs

-- | CSVの各カラムの最大桁数のリストを取得
maxLengths :: CSV -> [Int]
maxLengths = foldl (\acc xs -> zipWith max acc (map stringWidth xs)) [0..]

-- | 指定された桁数になるようにスペースを埋める
padRight :: String -> Int -> String
padRight s n = s ++ replicate (n - stringWidth s) ' '

-- | 文字列の桁数（半角=1、全角=2）を取得
stringWidth :: String -> Int
stringWidth = foldl (\acc c -> acc + charWidth c) 0

-- | 文字の桁数（半角=1、全角=2）を取得
charWidth :: Char -> Int
charWidth c
    | isHalf c  = 1
    | otherwise = 2

-- | 半角か判定
isHalf :: Char -> Bool
isHalf c = 128 > ord c
