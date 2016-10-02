module RLE where

import Data.List ( group, groupBy )

-- | ランレングス圧縮
--
-- >>> rle "AAABBCCCCD"
-- "A3B2C4D1"
--
-- >>> rle "AAAAAAAAAAB"
-- "A10B1"
--
rle :: String -> String
rle = concatMap (\s -> head s : show (length s)) . group

-- | ランレングス復号
--
-- >>> rle' "A3B2C4D1"
-- "AAABBCCCCD"
--
-- >>> rle' "A10B1"
-- "AAAAAAAAAAB"
--
rle' :: String -> String
rle' = concatMap decode . (groupBy g)
  where
    decode    = \x -> let chr = head x
                          num = toNum (tail x)
                      in  take num $ repeat chr
    toNum   s = read s :: Int
    g         = \x y -> isAlpha x && isNum y
    isAlpha c = 'A' <= c && c <= 'Z'
    isNum   c = '0' <= c && c <= '9'
