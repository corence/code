
module Sample where

import Data.Maybe
import Data.List(foldl')

deleteFromList :: [a] -> Int -> [a]
deleteFromList (x:xs) index = if index > 0 then deleteFromList xs (index - 1) else xs

contains :: Eq a => a -> [a] -> Bool
contains target list = isJust $ findIndex target list

findIndex :: Eq a => a -> [a] -> Maybe Int
--findIndex target = (listToMaybe . filter (== target))

findIndex _ [] = Nothing
findIndex target (x:xs) =
    if x == target
        then Just 0
        else fmap (+ 1) (findIndex target xs)

findAllIndices :: Eq a => a -> [a] -> [Int]
findAllIndices target list = findAllIndicesImpl target 0 list

findAllIndicesImpl :: Eq a => a -> Int -> [a] -> [Int]
findAllIndicesImpl _ depth [] = []
findAllIndicesImpl target depth (x:xs) =
    if x == target
        then depth : others
        else others
            where others = findAllIndicesImpl target (depth + 1) xs

findAllIndices2 :: Eq a => a -> [a] -> [Int]
findAllIndices2 target list = foldl' checker (0, []) list
    where checker (depth, results) (x:xs) =
            if x == target
                then (depth + 1, depth : results)
                else (depth + 1, results)
