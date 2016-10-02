main = do
  let a = print "hello" -- アクションを束縛
  _ <- a -- アクション無いの値を取り出し
  _ <- a
  a -- 暗黙の取り出し
  a
