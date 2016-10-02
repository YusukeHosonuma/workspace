import Data.List
import Data.Char
import qualified Data.Map as Map
import Geometry.Sphere

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` heystack = any (needle `isPrefixOf`) (tails heystack)

encode :: Int -> String -> String
--encode offset msg = map (\c -> chr $ ord c + offset) msg
encode offset = map (chr . (+offset) . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

--phoneBook :: Map.Map String String
phoneBook = --Map.fromList $
  [("betty", "080-111")
  ,("bonnie", "080-222-1")
  ,("bonnie", "080-222-2")
  ,("bonnie", "080-222-3")
  ,("patsy", "080-333")
  ,("lucille", "080-444-1")
  ,("lucille", "080-444-2")
  ,("wendy", "080-555")
  ,("panny", "080-666")
  ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2
