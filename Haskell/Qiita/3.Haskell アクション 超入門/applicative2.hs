fib 0 = return 0
fib 1 = return 1

-- Applicative style
fib n | n > 1 =
  (+) <$> fib (n - 2) <*> fib (n - 1)

-- non-applicative style
-- fib n | n > 1 = do
--     a <- fib (n - 2)
--     b <- fib (n - 1)
--     return $ a + b

main = do
    print =<< fib 6
