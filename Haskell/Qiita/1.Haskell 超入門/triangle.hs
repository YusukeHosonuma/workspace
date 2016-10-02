
ls = [1..20]
pt = [(x, y, z) | x <- ls, y <- ls, z <- ls, x * x + y * y == z * z]

main = do
  print pt
