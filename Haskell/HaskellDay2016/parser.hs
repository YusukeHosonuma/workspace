#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

parser :: Parser (Text, Maybe Int)
parser = subcommand "byopt" "オプション形式で指定する"
            ((,) <$> optText "opt1" 'x' "オプションひとつめ"
                 <*> optional (optInt  "opt2" 'y' "オプションふたつめ"))
         <|> (,) <$> argText "arg1" "引数ひとつめ"
                 <*> optional (argInt  "arg2" "引数ふたつめ")

main = do
    (str, mInt) <- options "An argument parser test" parser
    echo str
    case mInt of
      Nothing  -> return ()
      Just n -> printf d n
