#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = do
    args <- arguments
    let dirname = head args
    let filepath = fromText dirname
    mkdir filepath
    touch $ fromText "tmp.txt"
    cp (fromText "tmp.txt") (fromText $ dirname <> "/tmp.txt")
