import Numeric

pz1 :: Integer
pz1 = succ $ last $ takeWhile (not . pz1') [10..]

pz1' n = (show n  == reverse (show n)) &&
         (showOct n "" == reverse (showOct n "")) &&
         (i2b n == reverse (i2b n))

i2b :: Integer -> String
i2b = concat . (map show) . reverse . i2b'

i2b' :: Integer -> [Integer]
i2b' 0 = []
i2b' n = mod n 2 : i2b' (div n 2)
