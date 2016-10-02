#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, unpack)
import Turtle

import ToArray

parser :: Parser (Maybe Text, Maybe Text, Bool)
parser = (,,) <$> optional (optText "type" 't' "Type (arr|dic) - default is arr")
              <*> optional (optText "lang" 'l' "Language (swift | objc | java) - default is swift")
              <*> switch "multi" 'm' "Multi-line style"

putTextLn :: Text -> IO ()
putTextLn = putStrLn . unpack

language :: Maybe Text -> Either Text Language
language Nothing = Right Swift -- default
language (Just lang) = case lang of
    "swift"   -> Right Swift
    "objc"    -> Right Objc
    "java"    -> Right Java
    otherwise -> Left $ "Not supported language type '" <> lang <> "'"

-- ./Main [--lang (swift | objc | java)] [--multi] < sample.txt
main = do
    (mType, mLang, isMulti) <- options "Array Literal Converter" parser
    let toLiteral = case mType of Nothing    -> toArrayLiteral
                                  Just "dic" -> toDictionaryLiteral
                                  otherwise  -> toArrayLiteral
    case language mLang of
        Left errMsg -> do
            putTextLn errMsg
            putTextLn "Usage: --help"
        Right lang -> do
            contents <- getContents
            putTextLn $ (toLiteral isMulti lang) . map pack . lines $ contents
