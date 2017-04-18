
module AutoHeap
( AutoHeap(..)
, Heap(..)
, add
, add_all
, dump
, from_list
, is_empty
, query
, remove_head
, size
, void
) where

import qualified Heap2 as Heap
import Heap2(Heap(..))

data AutoHeap a = AutoHeap (Heap.Comparator a) (Heap a)

void :: Heap.Comparator a -> AutoHeap a
void comparator = AutoHeap comparator Heap.void

is_empty :: AutoHeap a -> Bool
is_empty aheap = size aheap == 0

from_list :: (Heap.Comparator a) -> [a] -> AutoHeap a
from_list comparator list = add_all list (AutoHeap comparator Heap.void)

add :: a -> AutoHeap a -> AutoHeap a
add entry (AutoHeap comparator heap) = AutoHeap comparator (Heap.add comparator entry heap)

add_all :: [a] -> AutoHeap a -> AutoHeap a
add_all entries initial_aheap = foldr add initial_aheap entries

query :: AutoHeap a -> Maybe a
query (AutoHeap _ heap) = Heap.query heap

remove_head :: AutoHeap a -> AutoHeap a
remove_head (AutoHeap comparator heap) = AutoHeap comparator (Heap.remove_head comparator heap)

size :: AutoHeap a -> Int
size (AutoHeap _ heap) = Heap.size heap

dump :: Show a => AutoHeap a -> String
dump (AutoHeap _ heap) = Heap.dump heap
