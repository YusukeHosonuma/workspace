data Test = TestInt Int
          | TestStr String
          deriving Show

foo (TestInt   1) = "bar"
foo (TestStr "1") = "baz"
foo _             = "?"

main = do
  print $ foo $ TestInt 0
  print $ foo $ TestInt 1
  print $ foo $ TestStr "0"
  print $ foo $ TestStr "1"
