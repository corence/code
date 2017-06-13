
import Test.QuickCheck
import Data.Maybe
import Heap
import Debug.Trace
    
isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered (x:[]) = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

prop_heapInsertDeleteIsOrdered :: [Maybe Int] -> Bool
prop_heapInsertDeleteIsOrdered instructions = isOrdered (toList heap)
    where heap = foldr runStep NoHeap instructions
          runStep Nothing heap = deleteMin heap
          runStep (Just num) heap = insert num heap

expectedHeapSize :: Show a => [Maybe a] -> Int
expectedHeapSize = foldr count 0

count :: Maybe a -> Int -> Int
count Nothing 0 = 0
count Nothing n = n - 1
count q n = n + 1

prop_heapSize :: [Maybe Int] -> Bool
prop_heapSize instructions = length (toList heap) == expectedHeapSize instructions
    where heap = foldr runStep NoHeap instructions
          runStep Nothing heap = deleteMin heap
          runStep (Just num) heap = insert num heap

main = do
    quickCheckWith stdArgs { maxSuccess = 1500 } prop_heapInsertDeleteIsOrdered
    quickCheckWith stdArgs { maxSuccess = 1500 } prop_heapSize
