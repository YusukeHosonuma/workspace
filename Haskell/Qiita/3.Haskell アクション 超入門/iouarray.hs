import Data.Array.IO

main = do
  a <- newArray (0, 2) 0 :: IO (IOUArray Int Int)
  print =<< getElems a
  writeArray a 0 3
  print =<< readArray a 0
  writeArray a 1 6
  print =<< getElems a
  writeArray a 2 7
  print =<< getElems a
