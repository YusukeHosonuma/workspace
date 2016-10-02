import Data.IORef

counter = do
  c <- newIORef 0
  return $ do
    tmp <- readIORef c
    writeIORef c $ tmp + 1
    readIORef c

main = do
  f <- counter
  print =<< f
  print =<< f
  print =<< f
