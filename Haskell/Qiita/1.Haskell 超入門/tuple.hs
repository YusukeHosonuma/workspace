addsub x y = (x + y, x - y)
a = addsub 1 2
(a1, a2) = addsub 1 2

main = do
  print a
  print a1
  print a2

  let p2 = (1, 2)
  print $ fst p2
  print $ snd p2

  let p3 = (1, 2, 3)
  print p3
  let (_,_, p3z) = p3
  print p3z
