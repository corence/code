
module AutoHeap
( AutoHeap(..)
, add
, dump
, Heap(..)
, Heap.void
, query
, remove_head
, size
) where

import qualified Heap2 as Heap
import Heap2(Heap(..))

data AutoHeap a = AutoHeap (Heap.Comparator a) (Heap a)

add :: a -> AutoHeap a -> AutoHeap a
add entry (AutoHeap comparator heap) = AutoHeap comparator (Heap.add comparator entry heap)

query :: AutoHeap a -> Maybe a
query (AutoHeap _ heap) = Heap.query heap

remove_head :: AutoHeap a -> AutoHeap a
remove_head (AutoHeap comparator heap) = AutoHeap comparator (Heap.remove_head comparator heap)

size :: AutoHeap a -> Int
size (AutoHeap _ heap) = Heap.size heap

dump :: Show a => AutoHeap a -> String
dump (AutoHeap _ heap) = Heap.dump heap
