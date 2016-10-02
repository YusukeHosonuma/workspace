#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

lsPrintf = do
    file <- ls "."
    printf (fp%"\n") file

main = do
    lsPrintf & sh
