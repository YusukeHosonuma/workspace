{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main = do
    txt <- T.getContents
    let out = T.unlines . map (T.append " ☆ ") . T.lines $ txt
    T.putStr out
