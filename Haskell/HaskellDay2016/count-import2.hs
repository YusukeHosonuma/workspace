#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle
import qualified Control.Foldl as Fold

parser :: Parser (Maybe Text, Bool)
parser = (,) <$> optional (optText "dir" 'd' "Target directory")
             <*> switch  "show" 's' "Show module names."

xargsGrep :: Shell Turtle.FilePath -> Shell Text
xargsGrep files = do
    file <- files
    grep (prefix "import") (input file)

-- | import文をカウントして出力
wc :: Text -> IO ()
wc path = do
    empty
        & inproc "find" ([path] <> ["-name", "*.hs"])
        & inproc "xargs" ["grep", "^import "]
        & inproc "wc" ["-l"]
        & stdout

-- | モジュール名を列挙
dump :: Text -> IO ()
dump path = do
    let filepath = find (suffix ".hs") (fromText path)
    (xargsGrep filepath)
        & sed ("import " *> pure "")
        & sed ("qualified " *> pure "")
        & sed (begins (spaces <> "as") *> pure "")
        & sed (begins (spaces <> "(") *> pure "")
        & stdout

main = do
    (mDir, isShow) <- options "Count import." parser
    let path = case mDir of Nothing -> "."
                            Just p  -> p

    let sink = if isShow then dump else wc
    sink path
    return ()
