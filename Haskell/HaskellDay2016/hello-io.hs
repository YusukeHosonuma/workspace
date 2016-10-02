#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

nestedIO = do
    putStr "Hello, "
    return (putStrLn "I/O!")

main = do
    r1 <- nestedIO
    r2 <- r1
    return ()
