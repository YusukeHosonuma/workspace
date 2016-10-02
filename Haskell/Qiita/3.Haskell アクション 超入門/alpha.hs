import System.Random

randAlpha = getStdRandom $ randomR ('a', 'z')

main = do
  r <- randAlpha
  print r
  if r == 'z' then print "END" else main
