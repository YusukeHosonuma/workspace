import Data.IORef

main = do
  let loop i s | i <= 100 = do
        loop (i + 1) (s + i)
      loop _ s = return (s)
  ret <- loop 0 0
  print ret
