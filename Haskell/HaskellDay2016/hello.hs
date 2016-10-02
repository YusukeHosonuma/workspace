#!/usr/bin/env stack
-- stack runghc

main = do
    cs <- readFile "pwd.hs"
    putStrLn cs
