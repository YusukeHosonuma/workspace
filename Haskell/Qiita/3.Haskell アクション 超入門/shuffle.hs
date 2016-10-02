import System.Random

-- シャッフル
shuffle [] = return []
shuffle xs = do
  n <- getStdRandom $ randomR (0, length xs - 1) :: IO Int
  xs' <- shuffle $ take n xs ++ drop (n + 1) xs
  return $ (xs !! n) : xs'

-- ソート済みか
isSorted [x] = True
isSorted (x:y:ys) = x < y && isSorted (y:ys)

-- ボゴソート
bogosort xs = do
  xs' <- shuffle xs
  if isSorted xs'
    then return xs'
    else bogosort xs'

main = do
  xs <- shuffle [1..9]
  print xs
  print =<< bogosort xs
  -- print =<< shuffle [1..9]
  -- print $ isSorted [1..9]
  -- print $ isSorted [1,2,3,5,4]
