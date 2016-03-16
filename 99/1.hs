
myLast1 [] = error "nothing in that list"
myLast1 [x] = x
myLast1 (x:xs) = myLast1 xs

myButLast2 [] = error "nothing in that list either"
myButLast2 [x] = error "not quite enough in that list"
myButLast2 (x:(_:[])) = x
myButLast2 (x:xs) = myButLast2 xs

elementAt3 [] _ = error "and nothing in that list jesus"
elementAt3 (x:xs) 1 = x
elementAt3 (_:xs) n = elementAt3 xs (n-1)

reverse5 x = reverse5a x []

reverse5a [] x = x
reverse5a (x:xs) (rs) = reverse5a (xs) (x:rs)

data List7 t = Element7 t | Sublist7 [List7 t]

flatten7 (Element7 x) = [x]
flatten7 (Sublist7 []) = []
flatten7 (Sublist7 (x:xs)) = (flatten7 x) ++ (flatten7 (Sublist7 xs))

group9 [] = []
group9 (x:xs) = expand9(x) : group9 xs

expand9 (0, _) = []
expand9 (num, x) = x : expand9 (num - 1, x)

type RLE10 t = (Int, t)

rle10 :: Eq t => [t] -> [RLE10 t]
rle10 [] = []
rle10 (x:[]) = [(1, x)]
rle10 (x:xs) =
  if (rVal == x) || (rCount == 0)
      then (rCount + 1, x) : rxs
      else (1, x) : rx : rxs
  where (rx:rxs) = rle10 xs
        (rCount, rVal) = rx
        
runProblem :: (Eq t, Show t) => String -> t -> t -> IO ()
runProblem pid expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ pid ++ ": [" ++ show expected ++ "] != [" ++ show actual ++ "]"

main = do
  runProblem "1a" 4 $ myLast1 [1,2,3,4]
  runProblem "1b" 'z' $ myLast1 ['x', 'y', 'z']

  runProblem "2a" 3 $ myButLast2 [1,2,3,4]
  runProblem "2b" 'y' $ myButLast2 ['x', 'y', 'z']

  runProblem "3a" 2 $ elementAt3 [1,2,3] 2
  runProblem "3b" 'e' $ elementAt3 "haskell" 5
  
  runProblem "5a" "A man, a plan, a canal, panama!" $ reverse5 "!amanap ,lanac a ,nalp a ,nam A"
  runProblem "5b" [1,2,3,4] $ reverse5 [4,3,2,1]

  runProblem "7a" [1,2,3,4,5] $ flatten7 (Sublist7 [Element7 1, (Sublist7 [Element7 2, (Sublist7 [Element7 3, Element7 4]), Element7 5])])

  runProblem "8a" "abcade" $ map (\(count, val) -> val) (rle10 "aaaabccaadeeee")
  
  runProblem "9a" ["aaaa","b","cc","aa","d","eeee"] $ group9 (rle10 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
  
  runProblem "10a" [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] $ rle10 "aaaabccaadeeee"
