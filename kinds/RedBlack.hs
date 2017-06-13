module RedBlack where

type Contrastor = a -> a -> Ordering

data Tree Contrastor a = Leaf | Node Bool (Tree contrastor a) a (Tree contrastor a) -- Node redness, left, value, right

insert :: a -> Tree a -> Tree a
insert input Leaf = Node True Leaf input Leaf
insert input (Node redness left value right) = error ""


-- 1) leaf is black
-- 2) red nodes only have black children
-- 3) every simple path from a node to a descendant leaf contains the same number of black nodes.
-- insert:
--  - add the new node as red, possibly destroying the red-black property
--  - walk up the tree, restoring the property
