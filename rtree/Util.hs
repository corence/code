
module Util
( extract_each
, sortWith
, smoosh
, take_n
) where

import Data.List

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith extract list = sortBy order list
    where order x y = compare (extract x) (extract y)

-- extract_each: Given an array, return every pair of an element + the rest of the array (with order preserved). Probably O(n^2) and not short-circuitable.
-- Examples:
-- extract_each [1,2,3] = [(1, [2,3]), (2, [1,3]), (3, [1,2])]
-- extract_each [3] = [(3, [])]
-- extract_each [2,3] = [(2, [3]), (3, [2])]
extract_each :: [a] -> [(a, [a])]
extract_each [] = []
extract_each (key:elements) = (key, elements) : (map (\(k, es) -> (k, key : es)) $ extract_each elements)

-- take the first n (or less) elements of an array and return them as the left. The remainder should be returned as the right.
take_n :: Int -> [a] -> ([a], [a])
take_n _ [] = ([], [])
take_n 0 xs = ([], xs)
take_n n (x:xs) = (x:left, right)
    where (left, right) = take_n (n - 1) xs
    
-- like ++ but not picky about ordering, so hopefully faster
smoosh :: [a] -> [a] -> [a]
smoosh [] ys = ys
smoosh (x:xs) ys = smoosh xs (x : ys)
