
module RedBlack where

data Pair a = Pair a a
data Tree joiner a = Empty | Tree a (joiner (Tree joiner a))
data Tree0 a = Empty0 | Tree0 a (Pair (Tree0 a))

-- (,) is crappy for this because it's (* -> * -> *) but we want (* -> *)
--x = Tree 7 (Empty, (Tree 6 (Empty, Empty)))

y :: Tree Pair Int
y = Tree 7 (Pair Empty Empty)

--z :: Tree (,) Int
--z = Tree 7 (Empty, Empty)

a = Tree 7 [Empty, Empty, Tree 7 [], Empty]

--b :: Tree (Tree Pair) Int
--b = Tree 7 (Tree 3 (Pair Empty Empty))

c :: Tree (Tree Pair) Int
c = Tree 7 Empty

--d :: Tree Tree0 Int
--d = Tree 7 (Tree0 3 (Pair Empty Empty))

e :: Tree Maybe Int
e = Tree 3 (Just $ Tree 4 Nothing)

{-
class RBTree tree where
    value :: tree -> a
    left :: tree -> tree
    right :: tree -> tree
    
data RedTree a = RedNode (BlackTree a) a (BlackTree a)
data BlackTree a = BlackNode | BlackBranch (RedTree a) a (RedTree a)

instance RBTree (RedTree a) where
    value (RedNode _ a _) = a
-}

data RBTree a = Leaf | RedNode (RBTree a) a (RBTree a) | BlackNode (RBTree a) a (RBTree a)

-- 1) leaf is black
-- 2) red nodes only have black children
-- 3) every simple path from a node to a descendant leaf contains the same number of black nodes.
-- insert:
--  - add the new node as red, possibly destroying the red-black property
--  - walk up the tree, restoring the property
