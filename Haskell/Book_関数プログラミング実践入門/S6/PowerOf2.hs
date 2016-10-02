module PowerOf2
    ( PowerOf2
    -- PowerOf2(..)
    , makePowerOf2
    , exponentPowerOf2
    ) where

newtype PowerOf2 = PowerOf2 Integer deriving (Eq, Show)

isPowerOf2 :: Integer -> Bool
isPowerOf2 n
    | n == 1 = True
    | n < 1 || odd n = False
    | otherwise = isPowerOf2 (n `div` 2)

makePowerOf2 :: Integer -> Maybe PowerOf2
makePowerOf2 n
    | isPowerOf2 n = Just (PowerOf2 n)
    | otherwise    = Nothing

exponentPowerOf2 :: PowerOf2 -> Integer
exponentPowerOf2 (PowerOf2 n) = exponentPowerOf2' 0 n where
    exponentPowerOf2' :: Integer -> Integer -> Integer
    exponentPowerOf2 r n
        | n == 1    = r
        | otherwise = exponentPowerOf2' (r + 1) (n `div` 2)
