data Foo = Foo { bar :: Int, baz :: String } deriving Show

main = do
  print $ Foo { bar = 1, baz = "a" }
  print $ Foo 2 "b"
