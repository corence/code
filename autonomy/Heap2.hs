
module Heap2
( Comparator(..)
, Heap(..)
, add
, dump
, fromList
, query
, remove_head
, size
, void
) where

data Heap a = Heap Int (Maybe a) (Heap a) (Heap a) -- size, key, value, left_child, right_child
type Comparator a = a -> a -> Ordering
void = Heap 0 Nothing void void

fromList :: Comparator a -> [a] -> Heap a
fromList compare entries = foldr (\entry heap -> add compare entry heap) void entries

toList :: Comparator a -> Heap a -> [a]
toList comparator heap
  = if size heap > 0
      then maybe_prepend (query heap) (toList comparator (remove_head comparator heap))
      else []

maybe_prepend :: Maybe a -> [a] -> [a]
maybe_prepend Nothing as = as
maybe_prepend (Just a) as = a : as

add :: Comparator a -> a -> Heap a -> Heap a
add compare entry (Heap p_size Nothing p_left p_right) = Heap (p_size + 1) (Just entry) p_left p_right
add compare entry (Heap p_size (Just p_entry) p_left p_right)
  | LT == compare entry p_entry = swap_value_with_parent
  | (size p_left <= size p_right) = add_to_left_child
  | otherwise = add_to_right_child
    where swap_value_with_parent = add compare p_entry (Heap p_size (Just entry) p_left p_right) -- the new entry is higher priority than the one in the heap, so swap them
          add_to_left_child = Heap (p_size + 1) (Just p_entry) (add compare entry p_left) p_right -- the left side of the tree is smaller so we'll insert the new entry there
          add_to_right_child = Heap (p_size + 1) (Just p_entry) p_left (add compare entry p_right) -- the right side of the tree is the better place to insert this new element

query :: Heap a -> Maybe a
query (Heap _ entry _ _) = entry

remove_head :: Comparator a -> Heap a -> Heap a
remove_head compare (Heap 0 Nothing p_left p_right) = error "can't remove from empty tree"
remove_head compare (Heap _ Nothing _ _) = error "this can't happen -- nonzero tree has Nothing at head"
remove_head compare (Heap 0 _ _ _) = error "this can't happen -- zero tree has non-nothing head"
remove_head compare (Heap p_size _ p_left p_right) = delete_empty_entry compare (Heap (p_size - 1) Nothing p_left p_right)

delete_empty_entry :: Comparator a -> Heap a -> Heap a
delete_empty_entry compare (Heap p_size Nothing p_left p_right)
  | size p_left == 0 && size p_right == 0 = void -- terminate if both children are empty
  | GT /= maybe_compare compare (entry p_left) (entry p_right) = Heap p_size (entry p_left) (remove_head compare p_left) p_right
  | otherwise = Heap p_size (entry p_right) p_left (remove_head compare p_right)
  where entry (Heap _ entry _ _) = entry

maybe_compare :: Comparator a -> Maybe a -> Maybe a -> Ordering
maybe_compare _ Nothing Nothing = EQ
maybe_compare _ Nothing _ = GT
maybe_compare _ _ Nothing = LT
maybe_compare compare (Just left) (Just right) = compare left right

size :: Heap a -> Int
size (Heap size _ _ _) = size

dump :: (Show a) => Heap a -> String
dump heap = dump_indented "" heap

dump_indented :: (Show a) => String -> Heap a -> String
dump_indented indent (Heap 0 _ _ _) = ""
dump_indented indent (Heap _ entry left right) = indent ++ show entry ++ "\n" ++ dump_indented ("  " ++ indent) left ++ dump_indented ("  " ++ indent) right
