import System.Random

dice :: IO Int
dice = getStdRandom $ randomR (1, 6)

showDice = do
  ret <- dice
  print ret
  return ret

main = do
  showDice
  showDice
  print =<< showDice
