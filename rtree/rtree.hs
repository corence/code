
module RTree
( RTree(..)
, Pos(..)
, Zone(..)
, RLeaf(..)
, RTree.insert
, void
, set_leaf
, RTree.print
, nearest_neighbour_scan
, nearest_neighbours
, nodes_in_zone
, node_is_empty
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
  | isNothing n_leaf = trase "insert 2 Nothing" $ set_leaf leaf node
  | length n_childs < capacity = trase ("insert 3 " ++ (show $ leaf_pos $ fromJust n_leaf)) $ node_add_child (node_from_leaf leaf) node -- make a new child and pass it in there
  | otherwise = trase ("insert 4 " ++ (show $ leaf_pos $ fromJust n_leaf)) $ insert_into_nearest_child capacity n_leaf leaf extracted_childs
    where RNode n_num_elements n_zone n_leaf n_childs = node
          RLeaf l_pos _ = leaf
          extracted_childs = extract_each n_childs
          childs_containing_new_pos = filter (\(c, _) -> Zone.contains l_pos (zone_from_node c)) extracted_childs
          sorted_childs_containing_new_pos = sortBy (\(c1, _) (c2, _) -> compare (node_num_elements c1) (node_num_elements c2)) childs_containing_new_pos

-- it's been extracted. insert into it, then assemble a new node around it.
insert_into_extracted :: Int -> Maybe (RLeaf v) -> RLeaf v -> RTreeChildIterator v -> RTree v
insert_into_extracted capacity parent_leaf baby_leaf (child, other_childs) = node_add_child new_child parent_node
    where parent_node = node_from_childs parent_leaf other_childs
          new_child = RTree.insert capacity baby_leaf child

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
node_zone (RNode _ zone _ _) = zone

node_is_empty (RNode 0 _ _ _) = True
node_is_empty _ = False

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

-- RTreeModifier v = (zone, [(leaf_is_scanned, leaf, unscanned childs, modified childs)])
type RTreeModifier v = (Zone, [(Bool, Maybe (RLeaf v), [RTree v], [RTree v])])

r_modifier :: Zone -> RTree v -> RTreeModifier v
r_modifier zone (RNode _ _ n_leaf n_childs) = r_modifier_advance (zone, [(False, n_leaf, n_childs, [])])

r_modifier_is_finished :: RTreeModifier v -> Bool
r_modifier_is_finished (_, ((True, _, [], _) : [])) = True
r_modifier_is_finished _ = False

r_modifier_to_node :: RTreeModifier v -> RTree v
r_modifier_to_node (_, []) = error "r_modifier_to_node needs entries"
r_modifier_to_node (zone, entry : entries)
  | null entries = entry_to_node entry
  | otherwise = r_modifier_to_node (zone, new_parent : ancestors)
    where (_, leaf, childs1, childs2) = entry
          (p_leaf_is_scanned, p_leaf, p_childs1, p_childs2) = parent
          parent : ancestors = entries
          new_parent = (p_leaf_is_scanned, p_leaf, [], (entry_to_node entry) : (p_childs1 ++ p_childs2))
          entry_to_node (_, leaf, childs1, childs2) = node_from_childs leaf (childs1 ++ childs2)

r_delete_zone_contents_via_modifier :: Zone -> RTree v -> RTree v
r_delete_zone_contents_via_modifier zone tree = r_modifier_delete_all (r_modifier zone tree)

r_modifier_delete_all :: RTreeModifier v -> RTree v
r_modifier_delete_all modifier
  | r_modifier_is_finished modifier = r_modifier_to_node modifier
  | otherwise = r_modifier_delete_all $ r_modifier_advance $ r_modifier_delete modifier -- delete the thing, advance it, and continue deleting

r_modifier_delete :: RTreeModifier v -> RTreeModifier v
r_modifier_delete (zone, (True, leaf, [], scanned) : entries) = (zone, (True, Nothing, [], scanned) : entries)
r_modifier_delete _ = error "r_modifier_delete is struggling to grasp how you got to be here"
          
-- 1) if there's an unscanned child, and it zone-checks, scan it
-- 2) if there's an unscanned child, and it doesn't zone-check, skip it
-- 3) if there's no more unscanned childs, and the leaf is not done, and the leaf zone-checks, use it
-- 4) if there's no more unscanned childs, and the leaf is not done, and the leaf doesn't zone-check, skip it
-- 5) if there's no more unscanned childs, and the leaf is already done, and it has a parent, fold it into its parent
-- 6) if there's no more unscanned childs, and the leaf is already done, and it has no parent, then terminate

r_modifier_advance :: RTreeModifier v -> RTreeModifier v
r_modifier_advance (zone, []) = error "this thing is empty -- that cannot happen ever"
r_modifier_advance (zone, (entry : entries))
  | length unscanned_childs > 0 = 
      let child:childs = unscanned_childs
          (RNode _ _ child_leaf child_childs) = child
      in
          if node_leaf_is_in_zone zone child
              then r_modifier_advance $ (zone, (False, child_leaf, child_childs, []) : (leaf_is_scanned, n_leaf, childs, modified_childs) : entries)
              else r_modifier_advance $ (zone, (leaf_is_scanned, n_leaf, childs, child : modified_childs) : entries)
  | not leaf_is_scanned =
      let advanced = (zone, (True, n_leaf, unscanned_childs, modified_childs) : entries) in
          if leafish_is_in_zone zone n_leaf
              then advanced
              else r_modifier_advance advanced
  | length entries > 0 =
      let parent : ancestors = entries
          (p_leaf_scanned, p_leaf, p_unscanned_childs, p_modified_childs) = parent
          new_node = node_from_childs n_leaf modified_childs
      in
          r_modifier_advance $ (zone, (p_leaf_scanned, p_leaf, p_unscanned_childs, new_node : p_modified_childs) : ancestors)
  | otherwise = (zone, [entry])
  where (leaf_is_scanned, n_leaf, unscanned_childs, modified_childs) = entry

-- todo: the following structure would also allow iteration to continue after deletion, so it would be superior:
-- (RTree v, [(Maybe RLeaf, [RTree v], [RTree v])])
-- so, instead of representing each ancestor as a full node but without the selected child, it would show them as:
--  * a value
--  * nodes that have already been iterated
--  * nodes that have not yet been iterated
-- advantage: we would no longer be tied to the internal representation of RTree
-- steps:
--  * tree is A [Aw [Awk, AWS], An [Any, Ant [Anti]], Ae [Aether, Aeon]]
--  * iterate for a zone and we get these nodes: [An Ant Anti Aeon]
--  * possibilities:
--  * 1) we iterate through the nodes, run a given Modify function _as we iterate_, and the mod function returns a result of type "v"
--  * 2) we iterate through the nodes, creating a structure that allows us to modify it later
--  * iterating through the result we could maybe have to walk through everything then collapse it. Thus the leaf:
--  (RTree v)
--  and the path:
--  RTreePath v = (Maybe (RLeaf v) this_node_value, [RTree v] unscanned_childs, [RTree v] unmatched_childs, [RTree v] matched_childs, RTreePath v modified_childs)

type RTreePath2 v = [RTreeAncestry v]
type RTreeAncestry2 v = ([RTree v], Maybe (RLeaf v), [RTree v])
type RTreeChildIterator2 v = ([RTree v], RTree v, [RTree v]) -- childs before, this child, childs after

-- finds leafs: (number of results requested) -> (they must be near this pos) -> (they must be in this zone) -> the tree -> results so far
--   for a given node:
--    * if the pos is not in this node's zone
--    *     then skip this node
--    *     else
--    *         if n_leaf is in the zone
--    *             then
--    *                 sorted_insert the new leaf into the other leafs
--    *                 if (length results) > num_requested
--    *                     drop a result
--    *                     shrink the query zone(!!) <- this is the key to performance
--    *         foldr (run this against each child, updating results and zone)
-- so basically:
--  - suppose you've been requested to find the 3 nearest neighbours to a given Pos. You may be given a Zone to narrow your search, or you may be given the Zone of the whole RTree. Either works.
--  - for each node:
--    - if it has a leaf in the current Zone, add it for consideration
--    - if we have found 3 or more poses in that Zone, shrink the zone to fit the ones we've found so far
--    - check inside each of the children, but only the ones that overlap the search zone
nearest_neighbours :: Int -> Pos -> RTree v -> [RLeaf v]
nearest_neighbours num_requested pos node = results
    where (_, results) = nearest_neighbour_scan num_requested pos node (node_zone node) []

nearest_neighbour_scan :: Int -> Pos -> RTree v -> Zone -> [RLeaf v] -> (Zone, [RLeaf v])
nearest_neighbour_scan num_requested pos node zone results
  | Zone.overlaps zone n_zone = trase ("nearest_neighbour_scan " ++ show num_requested ++ " " ++ show pos ++ " " ++ if (isJust n_leaf) then (\(Just (RLeaf pos _)) -> show pos) n_leaf else "" ++ show zone ++ " " ++ (show $ length results)) $ foldr (\child (zone, results) -> nearest_neighbour_scan num_requested pos child zone results) (adjusted_zone, adjusted_results) n_childs
  | otherwise = trase ("nearest_neighbour_scan end") $ (zone, results)
  where RNode n_num_elements n_zone n_leaf n_childs = node
        (adjusted_zone, adjusted_results) = if node_leaf_is_in_zone zone node
                                                then (new_zone, new_results)
                                                else (zone, results)
            where new_results = set_tail_length num_requested (sorted_insert (\leaf -> negate (leaf_dist_to_pos pos leaf)) (fromJust n_leaf) results)
                  new_zone = if length new_results >= num_requested then Zone.intersection zone (Zone.create_square pos radius) else zone
                  radius = maximum (leaf_pos (head new_results))
       
set_tail_length :: Int -> [a] -> [a]
set_tail_length _ [] = []
set_tail_length 0 xs = xs
set_tail_length num (_:xs) = set_tail_length (num - 1) xs

sorted_insert :: Ord b => (a -> b) -> a -> [a] -> [a]
sorted_insert _ x [] = [x]
sorted_insert evaluate x (e:es)
  | evaluate x <= evaluate e = x : e : es
  | otherwise = e : (sorted_insert evaluate x es)

leaf_dist_to_pos :: Pos -> RLeaf v -> Int
leaf_dist_to_pos pos (RLeaf l_pos _) = Zone.pos_distance pos l_pos

leaf_pos :: RLeaf v -> Pos
leaf_pos (RLeaf pos _) = pos

-- finds leafs: (number of results requested) -> (they must be near this pos) -> the tree
r_nearest_neighbours_1 :: Int -> Pos -> RTree v -> [RLeaf v]
r_nearest_neighbours_1 num_requested pos node
 | Zone.contains pos n_zone = filter_num num_requested (sorted_merge (leaf_dist_to_pos pos) result_groups)
 | otherwise = [] 
 where RNode n_num_elements n_zone n_leaf n_childs = node
       result_groups = case n_leaf of
                            Just leaf -> [leaf] : child_results
                            Nothing -> child_results
       child_results = map (\child -> r_nearest_neighbours_1 num_requested pos child) n_childs
       filter_num 0 _ = []
       filter_num num (x:xs) = x : (filter_num (num - 1) xs)
       
-- given a list of lists -- assuming those are already sorted! -- it merges all of them into one giant sorted monster list, using the given "evaluate" function to compare elements
sorted_merge :: Ord b => (a -> b) -> [[a]] -> [a]
sorted_merge _ [] = []
sorted_merge evaluate ([]:lists) = sorted_merge evaluate lists
sorted_merge _ (list:[]) = list
sorted_merge evaluate (list:[]:lists) = sorted_merge evaluate (list:lists)
sorted_merge evaluate (list1:list2:lists) = (head lower_list) : sorted_merge evaluate ((tail lower_list) : higher_list : lists)
    where (lower_list, higher_list) = if evaluate (head list1) <= evaluate (head list2) then (list1, list2) else (list2, list1)
        
-- another trans-ultimate iteration function. This one returns all the relevant leafs.
r_iterate_nomodify :: Zone -> RTree v -> [RLeaf v]
r_iterate_nomodify zone node
  | Zone.overlaps zone n_zone = all_values
  | otherwise = []
  where RNode n_num_elements n_zone n_leaf n_childs = node
        results_from_each_child = map (r_iterate_nomodify zone) n_childs
        leaf_value = if node_leaf_is_in_zone zone node
                         then [(fromJust n_leaf)]
                         else []
        all_values = leaf_value ++ concat results_from_each_child

-- another trans-ultimate iteration function. This one doesn't return a structure; all modifications must be done during iteration.
r_iterate_modify_null2 :: (RLeaf v -> Maybe (RLeaf v)) -> Zone -> RTree v -> RTree v
r_iterate_modify_null2 transmogrify zone node = fst $ r_iterate_modify transmog2 zone node
    where transmog2 leaf = let RLeaf pos value = leaf in (transmogrify leaf, value)

--r_iterate_modify_withval :: (RLeaf v -> (Maybe (RLeaf v), v)) -> Zone -> RTree v -> (RTree v, [v])
--r_iterate_modify_withval transmog zone tree =dd

r_iterate_modify_null :: (RLeaf v -> Maybe (RLeaf v)) -> Zone -> RTree v -> RTree v
r_iterate_modify_null transmogrify zone node
  | Zone.overlaps zone n_zone = new_node
  | otherwise = node
  where RNode n_num_elements n_zone n_leaf n_childs = node
        results_from_each_child = map (r_iterate_modify_null transmogrify zone) n_childs
        new_leaf = if node_leaf_is_in_zone zone node
                                     then transmogrify (fromJust n_leaf)
                                     else n_leaf
        new_node = node_from_childs new_leaf results_from_each_child

r_iterate_modify :: (RLeaf v -> (Maybe (RLeaf v), v)) -> Zone -> RTree v -> (RTree v, [v])
r_iterate_modify transmogrify zone node
  | Zone.overlaps zone n_zone = (new_node, new_values)
  | otherwise = (node, [])
  where RNode n_num_elements n_zone n_leaf n_childs = node
        results_from_each_child = map (r_iterate_modify transmogrify zone) n_childs
        (new_leaf, leaf_value) = if node_leaf_is_in_zone zone node
                                     then let (new_leaf, leaf_value) = transmogrify (fromJust n_leaf) in
                                        (new_leaf, [leaf_value])
                                     else (n_leaf, [])
        new_childs = map (\(new_child, _) -> new_child) results_from_each_child
        new_values = leaf_value ++ concat (map (\(_, value) -> value) results_from_each_child)
        new_node = node_from_childs new_leaf new_childs

r_iter_delete_all_in_zone :: Zone -> RTree v -> RTree v
r_iter_delete_all_in_zone zone tree = new_tree
    where delete (RLeaf pos value) = (Nothing, value)
          (new_tree, deleted_values) = r_iterate_modify delete zone tree

r_iter_delete_all_in_zone_null :: Zone -> RTree v -> RTree v
r_iter_delete_all_in_zone_null zone tree = r_iterate_modify_null delete zone tree
    where delete _ = Nothing

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

type RTreeIterator v = [RTree v]
type RTreeChildIterator v = (RTree v, [RTree v])

nodes_in_zone :: Zone -> RTree v -> RTreeIterator v -> [RTreeIterator v]
nodes_in_zone zone node parent_iterators
  | Zone.overlaps zone n_zone = iterators_from_this_node ++ (concat iterators_from_each_child)
  | otherwise = []
  where (RNode n_num_elements n_zone n_leaf n_childs) = node
        iterators_from_this_node = if node_leaf_is_in_zone zone node then [node : parent_iterators] else []
        iterators_from_each_child = map iterators_from_child (extract_each n_childs)
        iterators_from_child (child, other_childs) = nodes_in_zone zone child ((make_iterator_step other_childs) : parent_iterators)
        make_iterator_step childs = node_from_childs n_leaf childs

leafish_is_in_zone :: Zone -> Maybe (RLeaf v) -> Bool
leafish_is_in_zone zone (Just (RLeaf pos value)) = Zone.contains pos zone
leafish_is_in_zone zone Nothing = False

node_leaf_is_in_zone :: Zone -> RTree v -> Bool
node_leaf_is_in_zone zone (RNode _ _ n_leaf _) = leafish_is_in_zone zone n_leaf
                                    
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
