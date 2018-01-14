
module MergeSort
( sort
) where

sort :: Ord a => [a] -> [a]
sort list = repeatMerge (divide list)
    where divide = map pure

repeatMerge :: Ord a => [[a]] -> [a]
repeatMerge [] = []
repeatMerge (x:[]) = x
repeatMerge lists = repeatMerge (merge lists)

merge :: Ord a => [[a]] -> [[a]]
merge [] = []
merge (x:[]) = [x]
merge (x:y:xs) = mergeLists x y : merge xs

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  = if x < y
      then x : mergeLists xs (y:ys)
      else y : mergeLists (x:xs) ys
