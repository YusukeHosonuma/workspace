import Control.Applicative

-- 普通の（純粋な）関数
inc x   = x + 1
add x y = x + y

main = do
  print =<< inc <$> return 1
  print =<< add <$> return 1 <*> return 2
