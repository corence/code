
module RTree
( RTree(..)
, Pos(..)
, Zone(..)
, RLeaf(..)
, RTree.insert
, void
, set_leaf
, RTree.print
, nodes_in_zone
) where

import GHC.Exts
import Data.Maybe
import Debug.Trace
import Data.List

import qualified Zone
import Zone (Zone(..), Pos)

trase :: String -> a -> a
--trase string value = value
trase = trace

-- extract_each [1,2,3] = [(1, [2,3]), (2, [1,3]), (3, [1,2])]
-- extract_each [3] = [(3, [])]
-- extract_each [2,3] = [(2, [3]), (3, [2])]
extract_each :: [a] -> [(a, [a])]
extract_each [] = []
extract_each (key:elements) = (key, elements) : (map (\(k, es) -> (k, key : es)) $ extract_each elements)

indexed_elements :: Int -> [a] -> [(Int, a)]
indexed_elements _ [] = []
indexed_elements index (x:xs) = (index, x) : (indexed_elements (index + 1) xs)

remove_element :: Int -> [a] -> (a, [a])
remove_element _ [] = error "remove_element: index too big"
remove_element 0 (x:xs) = (x, xs)
remove_element n (x:xs) = (y, x : ys)
    where (y, ys) = remove_element (n - 1) xs

iterate_childs :: [RTree v] -> [RTreeChildIterator v]
iterate_childs = extract_each

-- RNode num_elements zone childs | RLeaf pos value
data RTree v = RNode Int Zone (Maybe (RLeaf v)) [RTree v] deriving (Show, Eq)
data RLeaf v = RLeaf Pos v deriving (Show, Eq)
void = RNode 0 ZVoid Nothing []

print :: (Show v) => Int -> RTree v -> String
print depth node = concat (replicate depth "  ")
                        ++ "[" ++ show n_num_elements ++ "] "
                        ++ "<" ++ show n_zone ++ "> "
                        ++ show n_leaf ++ "\n"
                        ++ concat (map (RTree.print (depth+1)) n_childs)
                        where (RNode n_num_elements n_zone n_leaf n_childs) = node

set_leaf :: RLeaf v -> RTree v -> RTree v
set_leaf leaf node = RNode (n_elements + 1) (Zone.extend l_pos n_zone) (Just leaf) n_childs
    where (RNode n_elements n_zone Nothing n_childs) = node
          (RLeaf l_pos _) = leaf

node_add_child :: RTree v -> RTree v -> RTree v
node_add_child (RNode 0 _ _ _) node = trase "node_add_child 1" $ node -- if he tries to insert an empty node, just ignore him
node_add_child child node = trase "node_add_child 2" $ RNode (c_num_elements + n_num_elements) (Zone.add c_zone n_zone) n_leaf (sorted_insert_by_num_elements child n_childs)
  where (RNode n_num_elements n_zone n_leaf n_childs) = node
        (RNode c_num_elements c_zone c_leaf c_childs) = child
                        
sorted_insert_by_num_elements :: RTree v -> [RTree v] -> [RTree v]
sorted_insert_by_num_elements element [] = [element]
sorted_insert_by_num_elements element  (n:ns) = if e_num_elements <= n_num_elements
                                                    then element : n : ns
                                                    else n : (sorted_insert_by_num_elements element ns)
  where (RNode e_num_elements e_zone e_leaf e_childs) = element
        (RNode n_num_elements n_zone n_leaf n_childs) = n

insert :: Int -> RLeaf v -> RTree v -> RTree v
insert capacity leaf node
  | length childs_containing_new_pos > 0 = trase "insert 1" $ insert_into_extracted capacity n_leaf leaf (head sorted_childs_containing_new_pos)
  | isNothing n_leaf = trase "insert 2" $ set_leaf leaf node
  | length n_childs < capacity = trase "insert 3" $ node_add_child (node_from_leaf leaf) node -- make a new child and pass it in there
  | otherwise = trase "insert 4" $ insert_into_nearest_child capacity n_leaf leaf extracted_childs
    where RNode n_num_elements n_zone n_leaf n_childs = node
          RLeaf l_pos _ = leaf
          extracted_childs = extract_each n_childs
          childs_containing_new_pos = filter (\(c, _) -> Zone.contains l_pos (zone_from_node c)) extracted_childs
          sorted_childs_containing_new_pos = sortBy (\(c1, _) (c2, _) -> compare (node_num_elements c1) (node_num_elements c2)) childs_containing_new_pos

-- it's been extracted. insert into it, then assemble a new node around it.
insert_into_extracted :: Int -> Maybe (RLeaf v) -> RLeaf v -> RTreeChildIterator v -> RTree v
insert_into_extracted capacity parent_leaf baby_leaf (child, other_childs) = node_add_child new_child parent_node
    where parent_node = node_from_childs parent_leaf other_childs
          new_child = node_add_child (node_from_leaf baby_leaf) child

nearest_extracted_child :: Pos -> [RTreeChildIterator v] -> RTreeChildIterator v
nearest_extracted_child pos extracted_childs = head $ sortBy comparator extracted_childs
    where comparator = (\(c1, _) (c2, _) -> compare (distance_to_zone pos (zone_from_node c1)) (distance_to_zone pos (zone_from_node c2))) 

distance_to_zone :: Pos -> Zone -> Int
distance_to_zone pos zone
    = max 0 (x1 - x)
    + max 0 (y1 - y)
    + max 0 (x - x2)
    + max 0 (y - y2)
    where [x, y] = pos
          Zone [x1, y1] [x2, y2] = zone

insert_into_nearest_child :: Int -> Maybe (RLeaf v) -> RLeaf v -> [RTreeChildIterator v] -> RTree v
insert_into_nearest_child capacity parent_leaf baby_leaf extracted_childs = insert_into_extracted capacity parent_leaf baby_leaf (nearest_extracted_child l_pos extracted_childs)
    where RLeaf l_pos _ = baby_leaf

zone_from_leaf (RLeaf l_pos _) = Zone l_pos l_pos
zone_from_node (RNode _ zone _ _) = zone

node_from_leaf leaf = RNode 1 (zone_from_leaf leaf) (Just leaf) []

node_num_elements (RNode num _ _ _) = num

type RTreeIterator v = [RTree v]
type RTreeChildIterator v = (RTree v, [RTree v])

-- (Target node, [(parent, index of next child)
-- eg suppose Q has the target leaf node
-- tree is A [B C D] -> C [F G H] -> F [L M N] -> N [P Q R]
-- this iterator is: (Q, [(N, 1), (F, 2), (C, 0), (A, 1)])
type RTreePath v = (Maybe (RTree v), RTreeAncestry v)
type RTreeAncestry v = [(RTree v, Int)]

-- this is intended to be the arch-ultimate lookup function.
-- in addition to the things nodes_in_zone can do, it also:
--  -- allows iteration to resume after deleting an element
--  -- is way less confusing

paths_in_zone :: Zone -> RTree v -> RTreeAncestry v -> [RTreePath v]
paths_in_zone zone node ancestry
  | Zone.overlaps zone n_zone = paths_from_this_leaf ++ (concat paths_from_each_child)
  | otherwise = []
  where (RNode n_num_elements n_zone n_leaf n_childs) = node
        paths_from_this_leaf = if node_leaf_is_in_zone zone node then [(Just node, ancestry)] else []
        paths_from_each_child = map paths_from_child (indexed_elements 0 n_childs)
        paths_from_child (index, child) = paths_in_zone zone child ((node, index) : ancestry)
        
path_remove :: RTreePath v -> RTreePath v
path_remove (Nothing, ancestors) = (Nothing, ancestors)
path_remove (Just element, (parent, index) : ancestors) = (Nothing, (new_parent, index) : new_ancestors)
  where RNode p_num_elements p_zone p_leaf p_child = parent
        RNode e_num_elements e_zone e_leaf e_child = element
        new_parent = node_remove_index index parent
        new_ancestors = path_adjust parent ancestors 

path_adjust :: RTree v -> RTreeAncestry v -> RTreeAncestry v
path_adjust _ [] = []
path_adjust node ((parent, index) : ancestors) = (new_parent, index) : path_adjust new_parent ancestors
    where new_parent = node_add_child node (node_remove_index index parent)
 
 -- element becomes Nothing
 -- parent loses it
 -- parent rebuilds
 -- grandparent realigns
 
node_remove_index :: Int -> RTree v -> RTree v
node_remove_index index node = node_from_childs n_leaf new_childs
    where RNode _ _ n_leaf n_childs = node
          (_, new_childs) = remove_element index n_childs

-- this is intended to be the ultimate Lookup function that will be the basis for all operations except Add.
-- It will return every match, "butterflied":
-- suppose A is the root RTree; one of its children is B, which has a child C, which has another child D.
-- D has the leaf we're searching for, and it also has children.
-- We'd return this as one of the list elements in the result:
-- ([D, C without D, B without C, A without B])

-- todo: the following structure would also allow iteration to continue after deletion, so it would be superior:
-- (RTree v, [(Maybe RLeaf, [RTree v], [RTree v])])
-- so, instead of representing each ancestor as a full node but without the selected child, it would show them as:
--  * a value
--  * nodes that have already been iterated
--  * nodes that have not yet been iterated
-- advantage: we would no longer be tied to the internal representation of RTree
nodes_in_zone :: Zone -> RTree v -> RTreeIterator v -> [RTreeIterator v]
nodes_in_zone zone node parent_iterators
  | Zone.overlaps zone n_zone = iterators_from_this_node ++ (concat iterators_from_each_child)
  | otherwise = []
  where (RNode n_num_elements n_zone n_leaf n_childs) = node
        iterators_from_this_node = if node_leaf_is_in_zone zone node then [node : parent_iterators] else []
        iterators_from_each_child = map iterators_from_child (extract_each n_childs)
        iterators_from_child (child, other_childs) = nodes_in_zone zone child ((make_iterator_step other_childs) : parent_iterators)
        make_iterator_step childs = node_from_childs n_leaf childs

node_leaf_is_in_zone :: Zone -> RTree v -> Bool
node_leaf_is_in_zone zone node = case n_leaf of
                                        Just (RLeaf l_pos _) -> Zone.contains l_pos zone
                                        Nothing -> False
                                    where (RNode _ _ n_leaf _) = node
                                    
node_from_childs :: Maybe (RLeaf v) -> [RTree v] -> RTree v
node_from_childs leaf childs = RNode (num_leaf_elements + num_child_elements) new_zone leaf childs
    where num_leaf_elements = if isJust leaf then 1 else 0
          num_child_elements = sum $ map (\(RNode n _ _ _) -> n) childs
          node_zones = map (\(RNode _ zone _ _) -> zone)
          combine_node_zones nodes = Zone.amalgamate (node_zones nodes)
          new_zone = case leaf of
                                    Just (RLeaf pos _) -> Zone.extend pos (combine_node_zones childs)
                                    Nothing -> combine_node_zones childs

remove :: RTreeIterator v -> RTree v
remove [] = error "can't remove empty iterator"
remove (node : parents) = merge parents

merge :: RTreeIterator v -> RTree v
merge [] = void
merge (node : []) = node
merge (node : parent : parents) = merge $ (node_add_child node parent) : parents

value :: RTreeIterator v -> v
value [] = error "no value on empty iterator"
value ((RNode _ _ n_leaf _) : _) = case n_leaf of
                                                    Just (RLeaf _ value) -> value
                                                    Nothing -> error "no value in an empty leaf"
