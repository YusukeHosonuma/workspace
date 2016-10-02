#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = do
    input "sample.txt" & inshell "uniq" & shell "wc -l"
    select ["To UNIX", "From Haskell"] & output "sample2.txt"
