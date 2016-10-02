import System.Random

randAlpha = getStdRandom $ randomR ('A', 'Z')

main = do
  r <- randAlpha
  print r
  print =<< randAlpha -- 関数 <-- アクション(値)
  randAlpha >>= print -- アクション(値) --> 関数
