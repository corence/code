-- I always do this at some point after the students have studied binary heaps (the kind typically used in heapsort). I briefly review the priority queue operations binary heaps support (insert, findMin, and deleteMin) and the basic idea of heap-ordered trees. Then, I introduce the idea of merging two heaps. With binary heaps, this takes O(N) time, or even O(N log N) time, if done naively. I ask them to design a new data structure to support merging in O(log n) time, while the other priority queue operations run in the same bounds as for binary heaps, O(1) for findMin and O(log N) for insert and deleteMin. I suggest that they use heap-ordered binary trees (real trees, rather than simulating trees using arrays like in binary heaps), but that they can modify these trees in any way that is helpful.

-- With heap-ordered trees, findMin is trivial—it just returns the root. To get their creative juices flowing, I then ask them to implement insert and deleteMin in terms of merge. This is also pretty easy: insert creates a new singleton tree and calls merge; deleteMin discards the root and calls merge on the two children of the root. So the whole problem boils down to merging two trees efficiently.

module Heap where

data Heap a = NoHeap | Heap Int (Heap a) a (Heap a)

findMin :: Heap a -> Maybe a
findMin NoHeap = Nothing
findMin (Heap _ _ value _) = Just value

merge :: Ord a => Heap a -> Heap a -> Heap a
merge NoHeap x = x
merge x NoHeap = x
merge left right = mergeIntoSubheap receiver giver
  where (Heap _ left1 value1 right1) = left
        (Heap _ left2 value2 right2) = right
        (receiver, giver) = if value1 <= value2 then (left, right) else (right, left)
        mergeIntoSubheap (Heap quantity left value right) giver = if size left <= size right
            then Heap (quantity + size giver) (merge left giver) value right
            else Heap (quantity + size giver) left value (merge right giver)

size :: Heap a -> Int
size NoHeap = 0
size (Heap size _ _ _) = size

insert :: Ord a => a -> Heap a -> Heap a
insert input = merge (Heap 1 NoHeap input NoHeap)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin NoHeap = NoHeap
deleteMin (Heap _ left _ right) = merge left right

toList :: Ord a => Heap a -> [a]
toList heap = maybe [] (: toList (deleteMin heap)) (findMin heap)

test :: IO ()
test = do
    let h1 = ((insert 5) . (insert 7) . (insert 3) . (insert 4) . (insert 5)) NoHeap
    let h2 = ((insert 5) . (insert 7) . (insert 3) . (insert 4) . (deleteMin) . (insert 5)) NoHeap -- note it's using (.) so these are processed right to left
    let h3 = ((insert 5) . (deleteMin) . (insert 7) . (insert 3) . (insert 4) . (insert 5)) NoHeap
    putStrLn $ show $ toList $ h1
    putStrLn $ show $ toList $ h2
    return ()
