#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

-- main = do
--     let greeting = "Hello, world!"
--     echo greeting

dateStr = do
    let title = "now: "
    now <- date
    return (title <> show now)

main = do
    str <- dateStr
    printf ("My name is "%s%". "%d%" years old.\n") "shu1" 0
    putStrLn str
