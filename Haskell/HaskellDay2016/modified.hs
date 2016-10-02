#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = do
    args <- arguments
    let filepath = head args
    let file = fromText filepath
    modified <- datefile file
    echo $ repr modified
