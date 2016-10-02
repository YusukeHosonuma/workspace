fact 0 = 1                -- 基底部
fact n = n * fact (n - 1) -- 再帰部

main = do
  print $ fact 5
