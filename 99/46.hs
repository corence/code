
type RLE10 t = (Integer, t)

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

type Huff50 t = (t, String)

swap50 [] = []
swap50 ((x, count):xs) = (count, x) : (swap50 xs)

huffman50 :: Ord t => [RLE10 t] -> [Huff50 t]
huffman50 [] = []
huffman50 frequencies = sort50 (\(val, _) -> val) (huff50 (huffsort50 frequencies))

huffsort50 = sort50 (\(count, value) -> -count)

sort50 :: Ord comp => (t -> comp) -> [t] -> [t]
sort50 pred list = merge50 pred (explode50 list)

explode50 :: [t] -> [[t]]
explode50 [] = []
explode50 (x:xs) = [x] : explode50 xs

merge50 :: Ord comp => (t -> comp) -> [[t]] -> [t]
merge50 pred [] = []
merge50 pred (x:[]) = x
merge50 pred lists = merge50 pred (mergeStep50 pred lists)

mergeStep50 :: Ord comp => (t -> comp) -> [[t]] -> [[t]]
mergeStep50 pred [] = []
mergeStep50 pred (x:[]) = [x]
mergeStep50 pred (x:y:xs) = (mergeTwo50 pred x y) : mergeStep50 pred xs

mergeTwo50 :: Ord comp => (t -> comp) -> [t] -> [t] -> [t]
mergeTwo50 pred [] ys = ys
mergeTwo50 pred xs [] = xs
mergeTwo50 pred (x:xs) (y:ys) =
  if pred x < pred y
      then x : mergeTwo50 pred xs (y:ys)
      else y : mergeTwo50 pred (x:xs) ys

huff50 :: [RLE10 t] -> [Huff50 t]
huff50 ((count, val):xs) = [(val, "pig")]

main = do
  runProblem "50a" [(45, 'a'), (16, 'd'), (13, 'b'), (12, 'c'), (9, 'e'), (5, 'f')] $ huffsort50 $ swap50 [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  runProblem "50b" [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")] $ huffman50 $ swap50 [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  
