import System.Random

dice :: IO Int
dice = getStdRandom $ randomR (1, 6)

main = do
  print =<< dice
  print =<< dice
  print =<< dice
