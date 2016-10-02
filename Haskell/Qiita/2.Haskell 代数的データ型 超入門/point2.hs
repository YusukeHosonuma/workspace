data Point = Point { px :: Int, py :: Int } deriving Show
data Rect = Rect { x :: Int, y :: Int, w :: Int, h :: Int } deriving Show

contains (Rect x y w h) (Point px py) =
  x <= px && px < (x + w) &&
  y <= py && py < (y + h)

main = do
  print $ contains Rect { x = 2, y = 2, w = 3, h = 3 } Point { px = 1, py = 1 }
  print $ contains Rect { x = 2, y = 2, w = 3, h = 3 } Point { px = 2, py = 2 }
  print $ contains Rect { x = 2, y = 2, w = 3, h = 3 } Point { px = 3, py = 3 }
  print $ contains Rect { x = 2, y = 2, w = 3, h = 3 } Point { px = 4, py = 4 }
  print $ contains Rect { x = 2, y = 2, w = 3, h = 3 } Point { px = 5, py = 5 }
