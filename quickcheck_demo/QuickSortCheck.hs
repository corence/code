
--import QuickSort
import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: [Int] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

main = quickCheck prop_idempotent
