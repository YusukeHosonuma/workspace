
import System.Environment

import Arguments
import CSV

-- | 使用するフォーマット関数を特定
formatter :: OutputValue -> CSVFormatter
formatter Normal   = formatCSVLines
formatter Markdown = formatCSVMarkdown

-- | 使用するデリミタを特定
delimiter :: FormatValue -> Char
delimiter CSV = ','
delimiter TSV = '\t'

-- | タイプセーフな実行
dispatch :: (FormatValue, OutputValue) -> String -> IO ()
dispatch (formatValue, outputValue) contents = do
    let result = unlines . fmt . parseCSV dlm $ contents
    putStrLn result
        where
            fmt = formatter outputValue
            dlm = delimiter formatValue

-- | ./Main --format (csv | tsv) --output (normal | markdown)
main = do
    maybeOptions <- fmap parseOptions getArgs
    contents <- getContents
    case maybeOptions of
        Just options ->
            dispatch options contents
        Nothing -> do
            putStrLn "Invalid arguments."
            putStrLn "Usage: ./Main --format (csv | tsv) --output (normal | markdown)"
