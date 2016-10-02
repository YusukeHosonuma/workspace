bswap [x] = [x]
bswap (x:xs)
  |
  where
    (y:ys) = bswap xs
