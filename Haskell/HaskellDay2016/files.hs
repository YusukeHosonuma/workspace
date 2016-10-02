#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = do
    ls "." & fmap (format fp) & sed ("./" *> pure "") & stdout
    return ()
