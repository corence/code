
module Heap
( Heap(..)
, add
, query
, remove_head
, size
, void
) where

type Entry key value = (key, value)
data Heap key value = Heap Int (Maybe (key, value)) (Heap key value) (Heap key value) -- size, key, value, left_child, right_child
void = Heap 0 Nothing void void

add :: Ord key => (key, value) -> Heap key value -> Heap key value
add entry (Heap p_size Nothing p_left p_right) = Heap (p_size + 1) (Just entry) p_left p_right
add entry (Heap p_size (Just p_entry) p_left p_right)
  | higher_priority (Just entry) (Just p_entry) = add p_entry (Heap p_size (Just entry) p_left p_right) -- the new entry is higher priority than the one in the heap, so swap them
  | (size p_left <= size p_right) = Heap (p_size + 1) (Just p_entry) (add entry p_left) p_right -- the left side of the tree is smaller so we'll insert the new entry there
  | otherwise = Heap (p_size + 1) (Just p_entry) p_left (add entry p_right) -- the right side of the tree is the better place to insert this new element

higher_priority :: Ord key => Maybe (Entry key value) -> Maybe (Entry key value) -> Bool
higher_priority _ Nothing = True
higher_priority Nothing _ = False
higher_priority (Just (left_key, _)) (Just (right_key, _)) = left_key < right_key

query :: Heap key value -> Maybe value
query (Heap _ entry _ _) = fmap snd entry

remove_head :: Ord key => Heap key value -> Heap key value
remove_head (Heap 0 Nothing p_left p_right) = error "can't remove from empty tree"
remove_head (Heap p_size p_entry p_left p_right) = delete_empty_entry (Heap (p_size - 1) Nothing p_left p_right)

delete_empty_entry :: Ord key => Heap key value -> Heap key value
delete_empty_entry (Heap p_size Nothing p_left p_right)
  | size p_left == 0 && size p_right == 0 = void -- terminate if both children are empty
  | higher_priority (entry p_left) (entry p_right) = Heap p_size (entry p_left) (remove_head p_left) p_right -- 
  | otherwise = Heap p_size (entry p_right) p_left (remove_head p_right)
  where entry (Heap _ entry _ _) = entry

size :: Heap key value -> Int
size (Heap size _ _ _) = size

values :: Ord key => Heap key value -> [value]
--values heap = maybe [] (: values (remove_head heap)) (query heap)
values = map snd . toList

toList :: Ord key => Heap key value -> [(key, value)]
toList heap@(Heap _ entry _ _) = maybe [] (: toList (remove_head heap)) entry
