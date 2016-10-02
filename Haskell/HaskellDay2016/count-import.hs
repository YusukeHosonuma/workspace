#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle
import qualified Control.Foldl as Fold

xargsGrep :: Shell Turtle.FilePath -> Shell Text
xargsGrep files = do
    file <- files
    grep (prefix "import") (input file)

main = do
    args <- arguments
    let dirname = head args

    -- inshell + formatを使った例：
    let cmd = format ("find "%s%" -name \\*.hs | xargs grep '^import ' | wc -l") dirname
    empty & inshell cmd & stdout

    -- inprocを使った例：
    empty
        & inproc "find" ([dirname] <> ["-name", "*.hs"])
        & inproc "xargs" ["grep", "^import "]
        & inproc "wc" ["-l"]
        & stdout

    -- 外部コマンドを使わない例：
    let filepath = find (suffix ".hs") (fromText dirname)
    -- view filepath
    view (Turtle.fold (xargsGrep filepath) Fold.length)
    return ()
