{-# LANGUAGE OverloadedStrings #-}

module ToArray
( Language (..)
, toArrayLiteral
, toDictionaryLiteral
) where

import Data.Text (pack, unpack)
import Turtle

data Language = Swift | Objc | Java deriving (Show, Enum, Bounded)

-- | 配列のリテラルに変換（Readerモナド）
toArrayLiteral :: Bool -> Language -> [Text] -> Text
toArrayLiteral multi = do
    let arrayJoin' = arrayJoin multi
    toStringL <- stringLiteral
    joinElem  <- arrayJoin'
    closeArr  <- arrayClose
    return (closeArr . joinElem . map toStringL)

-- | ディクショナリのリテラルに変換（Readerモナド）
toDictionaryLiteral :: Bool -> Language -> [Text] -> Text
toDictionaryLiteral multi = do
    toKeyValue <- keyValueLiteral
    joinElem   <- arrayJoin multi
    closeDic   <- dicClose
    return (closeDic . joinElem . map toKeyValue)

-- | Key -> Value なリテラルに変換
keyValueLiteral :: Language -> Text -> Text
keyValueLiteral lang text =
    let (key:val:[]) = words $ unpack text
        key' = stringLiteral lang (pack key)
        val' = stringLiteral lang (pack val)
    in case lang of Java      -> error "Java is not supported."
                    otherwise -> key' <> ": " <> val'

-- | 要素を,で連結
arrayJoin :: Bool -> Language -> [Text] -> Text
arrayJoin False _    = joinM ", "
arrayJoin True  lang = joinM $ case lang of Objc      -> "\n, "
                                            otherwise -> "\n,"

-- | 文字列リテラルへ
stringLiteral :: Language -> Text -> Text
stringLiteral Objc = wrap' "@\"" "\""
stringLiteral _    = wrap "\""

-- | 配列リテラルの外側
arrayClose :: Language -> Text -> Text
arrayClose Objc = wrap' "@[" "]"
arrayClose _    = wrap' "[" "]"

-- | ディクショナリの外側
dicClose :: Language -> Text -> Text
dicClose Objc  = wrap' "@{" "}"
dicClose Swift = wrap' "[" "]"
dicClose Java  = error ""

joinM :: (Monoid m) => m -> [m] -> m
joinM separator = foldl1 (\a b -> a `mappend` separator `mappend` b)

wrap :: Text -> Text -> Text
wrap t = wrap' t t

wrap' :: Text -> Text -> Text -> Text
wrap' t1 t2 s = t1 <> s <> t2
