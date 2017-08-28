
module DifferenceList where

data DifferenceList a = DNode (DifferenceList a) (DifferenceList a) | DLeaf [a]

addHead :: a -> DifferenceList a -> DifferenceList a
addHead a (DNode x y) = DNode (addHead a x) y
addHead a (DLeaf xs) = DLeaf (a : xs)

addTail :: a -> DifferenceList a -> DifferenceList a
addTail a (DNode x y) = DNode x (addTail a y)
addTail a (DLeaf ys) = DNode (DLeaf ys) (DLeaf [a])

append :: DifferenceList a -> DifferenceList a -> DifferenceList a
append = DNode

toList :: DifferenceList a -> [a]
toList dlist = toList' dlist []
    where toList' (DNode x y) result = toList' x (toList' y result)
          toList' (DLeaf (x:xs)) result = undefined
