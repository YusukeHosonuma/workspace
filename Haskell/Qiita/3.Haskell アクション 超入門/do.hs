import System.Random

randAlpha = getStdRandom $ randomR ('A', 'Z')

test = do
  r <- randAlpha
  print r

main = do
  test
  test
  test
