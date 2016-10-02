import Debug.Trace

test x = trace ("test " ++ show x) x

main = do
  traceIO $ show $ test 5
