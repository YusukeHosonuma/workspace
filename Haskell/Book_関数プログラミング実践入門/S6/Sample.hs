module Sample where

import PowerOf2

example :: IO ()
example = case makePowerOf2 3 of
    Just n  -> print (exponentPowerOf2 n)
    Nothing -> putStrLn "3 is invalid"
