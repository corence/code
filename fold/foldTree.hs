
main = pure ()

(&) = flip ($)

-- binary tree, with data in the nodes, with no duplicates
data NodeTree a = Leaf | Node Int (NodeTree a) a (NodeTree a)

-- binary tree, with data in the leafs, with no duplicates
data LeafTree a = EmptyTree | Meaf a | Mode Int (LeafTree a) (LeafTree a)

nAdd :: (a -> a -> Ordering) -> a -> NodeTree a -> NodeTree a
nAdd _ value Leaf = Node 1 Leaf value Leaf
nAdd comparator value (Node height left nValue right)
  = case comparator value nValue of
         LT -> Node (height + 1) (nAdd comparator nValue left) value right
         GT -> Node (height + 1) left value (nAdd comparator nValue right)
         EQ -> Node height left value right -- we replaced nValue

nToList :: NodeTree a -> [a]
nToList Leaf = []
nToList (Node _ left value right) = nToList left ++ [value] ++ nToList right

nFold :: (a -> b -> b) -> b -> NodeTree a -> b
nFold _ initial Leaf = initial
nFold func initial (Node _ left value right)
  = initial & nFold2 func left & func value & nFold2 func right
    where nFold2 func = flip (nFold func) -- flip it so the "initial" is the last param, so that we can thread the results through (&)

mFold :: (a -> b -> b) -> b -> LeafTree a -> b
mFold _ initial EmptyTree = initial
mFold func initial (Meaf value) = func value initial
mFold func initial (Mode _ left right)
  = initial & mFold2 func left & mFold2 func right
    where mFold2 func = flip (mFold func)
