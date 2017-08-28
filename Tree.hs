
module Tree where

-- this implements binary trees that have some kind of ordering, such as: binary search trees, binary heaps, maybe others

--data Tree k a = Node (Tree k a) k (Tree k a) | Leaf a
data Heap a = Node (Tree k a) k (Tree k a) | Leaf a

class BinaryTree t where
    empty :: BinaryTree t => t
    add :: (Ord a, BinaryTree t) => a -> t -> t
    toList :: (Ord a, BinaryTree t) => t -> [a]

instance BinaryTree Heap where
