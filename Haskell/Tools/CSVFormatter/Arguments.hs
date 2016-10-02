module Arguments
( FormatValue(..)
, OutputValue(..)
, parseOptions
) where

import Data.List

data Option = Format FormatValue | Output OutputValue

data FormatValue = CSV | TSV
data OutputValue = Normal | Markdown

parseOptions :: [String] -> Maybe (FormatValue, OutputValue)
parseOptions xs = do
    f <- extractFormat options
    o <- extractOutput options
    return (f, o)
        where options = map parseOption $ parseArgs xs

parseArgs :: [String] -> [[String]]
parseArgs = groupBy (\a b -> (isKey a) && (not $ isKey b))
    where isKey = isPrefixOf "-"

formatOption :: [String] -> Option
formatOption ["csv"] = Format CSV
formatOption ["tsv"] = Format TSV
formatOption _ = undefined

outputOption :: [String] -> Option
outputOption ["normal"]   = Output Normal
outputOption ["markdown"] = Output Markdown
outputOption _ = undefined

parseOption :: [String] -> Option
parseOption (key:args) = case key of
                        "--format" -> formatOption args
                        "--output" -> outputOption args

extract :: (Option -> Maybe a) -> [Option] -> Maybe a
extract f = foldl (\acc option -> case acc of Nothing -> f option
                                              Just x  -> Just x) Nothing

extractFormat :: [Option] -> Maybe FormatValue
extractFormat = extract (\option -> case option of Format x -> Just x
                                                   Output _ -> Nothing)

extractOutput :: [Option] -> Maybe OutputValue
extractOutput = extract (\option -> case option of Format _ -> Nothing
                                                   Output x -> Just x)
