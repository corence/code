
module RTree (
    module RTree
) where

--import Data.List.Ordered
import GHC.Exts
import Data.Maybe

-- extract_each [1,2,3] = [(1, [2,3]), (2, [1,3]), (3, [1,2])]
-- extract_each [3] = [(3, [])]
-- extract_each [2,3] = [(2, [3]), (3, [2])]
extract_each :: [a] -> [(a, [a])]
extract_each [] = []
extract_each (key:elements) = (key, elements) : (map (\(k, es) -> (k, key : es)) $ extract_each elements)

-- insert into void: create leaf
-- insert into child that contains this rect: do that
-- insert when no child contains this rect, and this node isnt full: add a new leaf child
-- otherwise: extend one of the childs

-- insert into node:
-- 0) is this node void? then just return a new leaf
-- 1) do any of the childs contain the new point? then insert into the one with least children. tiebreaker: arbitrary
-- 2) can this node take another child? add it as a new leaf
-- 3) otherwise: find the child with the rect nearest to the new point, and extend it

type Pos = [Int]
data Zone = ZVoid | Zone Pos Pos deriving (Show, Eq)

pos_to_zone :: Pos -> Zone
pos_to_zone pos = Zone pos pos

zone_contains :: Pos -> Zone -> Bool
zone_contains _ ZVoid = False
zone_contains pos (Zone nw se) = and (zipWith (<=) nw pos) && and (zipWith (>=) se pos)

zone_overlaps :: Zone -> Zone -> Bool
zone_overlaps ZVoid _ = False
zone_overlaps _ ZVoid = False
zone_overlaps (Zone [n1, w1] [s1, e1]) (Zone [n2, w2] [s2, e2]) = 
  not $ (e1 < w2) || (w1 > e2) || (s1 < n2) || (n1 > s2)

zone_extend :: Pos -> Zone -> Zone
zone_extend pos ZVoid = Zone pos pos
zone_extend pos (Zone nw se) = Zone most_nw most_se
  where most_nw = zipWith min nw pos
        most_se = zipWith max se pos

zone_add :: Zone -> Zone -> Zone
zone_add ZVoid zone = zone
zone_add zone ZVoid = zone
zone_add zone (Zone nw se) = zone_extend se (zone_extend nw zone)

-- RNode num_elements zone childs | RLeaf pos value
data RTree v = RNode Int Zone (Maybe (RLeaf v)) [RTree v] deriving (Show, Eq)
data RLeaf v = RLeaf Pos v deriving (Show, Eq)
r_void = RNode 0 ZVoid Nothing []

r_print :: (Show v) => Int -> RTree v -> String
r_print depth node = concat (replicate depth "  ")
                        ++ "[" ++ show n_num_elements ++ "] "
                        ++ show n_zone ++ " "
                        ++ show n_leaf ++ "\n"
                        ++ concat (map (r_print (depth+1)) n_childs)
                        where (RNode n_num_elements n_zone n_leaf n_childs) = node

r_set_leaf :: RLeaf v -> RTree v -> RTree v
r_set_leaf leaf node = RNode (n_elements + 1) (zone_extend l_pos n_zone) (Just leaf) n_childs
    where (RNode n_elements n_zone Nothing n_childs) = node
          (RLeaf l_pos _) = leaf

r_node_add_child :: RTree v -> RTree v -> RTree v
r_node_add_child (RNode 0 _ _ _) node = node -- if he tries to insert an empty node, just ignore him
r_node_add_child child node = RNode (c_num_elements + n_num_elements) (zone_add c_zone n_zone) n_leaf (sorted_insert_by_num_elements child n_childs)
  where (RNode n_num_elements n_zone n_leaf n_childs) = node
        (RNode c_num_elements c_zone c_leaf c_childs) = child

r_node_remove_first_child :: RTree v -> (RTree v, RTree v)
r_node_remove_first_child node = (n_child, RNode (n_num_elements - c_num_elements) n_zone n_leaf n_childs) -- we don't *have* to reduce the zone here, but it would be nice
  where (RNode n_num_elements n_zone n_leaf (n_child:n_childs)) = node
        (RNode c_num_elements c_zone c_leaf c_childs) = n_child
        
sorted_insert_by_num_elements :: RTree v -> [RTree v] -> [RTree v]
sorted_insert_by_num_elements element [] = [element]
sorted_insert_by_num_elements element  (n:ns) = if e_num_elements <= n_num_elements
                                                    then element : n : ns
                                                    else n : (sorted_insert_by_num_elements element ns)
  where (RNode e_num_elements e_zone e_leaf e_childs) = element
        (RNode n_num_elements n_zone n_leaf n_childs) = n

-- 0) remove the child
-- 1) insert into the child
-- 2) update num_elements, zones, and childs (childs must be sorted)
r_insert_into_first_child :: Int -> RLeaf v -> RTree v -> RTree v
r_insert_into_first_child capacity leaf node = r_node_add_child child_after node_without_child
    where (child_before, node_without_child) = r_node_remove_first_child node
          child_after = r_insert capacity leaf child_before

--r_insert_into_first_child capacity leaf node = RNode (n_elements + 1) (zone_extend l_pos n_zone) (sorted_insert_by_num_elements new_child n_childs)
  --where (RNode n_elements n_zone n_leaf (n_child : n_childs)) = node
        --(RLeaf l_pos _) = leaf
        --new_child = r_insert capacity n_child leaf

-- insertBagBy (a -> a -> Ordering) -> a -> [a] -> [a]

-- insert into node:
-- 0) does it fit into any of the childs? then insert into the one with least children. tiebreaker: arbitrary
-- 1) does this node have a leaf? if not, add it here
-- 2) can this node take another child? add it as a new child, and insert into that
-- 3) otherwise: find the child with the rect nearest to the new point, and extend it (actual implementation: insert into the first child)
r_insert :: Int -> RLeaf v -> RTree v -> RTree v
r_insert capacity leaf node
  | length n_childs_containing_new_pos > 0 = r_insert_into_first_child capacity leaf node -- there's a child node who matches the zone -- pass it down there
  | isNothing n_leaf = r_set_leaf leaf node -- add it to this node
  | length n_childs < capacity = r_insert_into_first_child capacity leaf (r_node_add_child r_void node) -- make a new child and pass it in there
  | otherwise = r_insert_into_first_child capacity leaf node -- pass it down the line for someone else to deal with
  -- | otherwise = error "what"
  -- | otherwise = node
    where (RNode n_elements n_zone n_leaf n_childs) = node
          (RLeaf l_pos _) = leaf
          n_childs_containing_new_pos = filter (\c -> zone_contains l_pos (r_node_zone c)) n_childs
          r_node_zone (RNode _ zone _ _) = zone

-- return all leaves in a zone
r_lookup_zone :: Zone -> RTree v -> [RLeaf v]
r_lookup_zone zone (RNode n_num_elements n_zone n_leaf n_childs) = case matching_leaf of
                                                                       Just leaf -> leaf : child_results
                                                                       Nothing -> child_results
    where child_results = if zone_overlaps zone n_zone
                              then concat $ map (r_lookup_zone zone) n_childs
                              else []
          matching_leaf = case n_leaf of
                              Just (RLeaf l_pos l_value) -> if zone_contains l_pos zone
                                                                then Just (RLeaf l_pos l_value)
                                                                else Nothing
                              Nothing -> Nothing

{-
r_lookup_pos :: Pos -> RTree v -> [RLeaf v]
r_lookup_pos pos (RNode n_num_elements n_zone n_leaf n_childs) = case matching_leaf of
                                                                      Just leaf -> leaf : child_results
                                                                      Nothing -> child_results
    where child_results = if zone_contains pos n_zone
                              then concat $ map (r_lookup_pos pos) n_childs
                              else []
          matching_leaf = case n_leaf of
                               Just (RLeaf l_pos l_value) -> if pos == l_pos
                                                                 then Just (RLeaf l_pos l_value)
                                                                 else Nothing
                               Nothing -> Nothing
-}

r_lookup_pos :: Pos -> RTree v -> [RLeaf v]
r_lookup_pos pos node = r_lookup_zone (Zone pos pos) node

-- find one node containing this leaf, and return it with all its ancestors
r_lookup_leaf :: Eq v => RLeaf v -> RTree v -> [RTree v]
r_lookup_leaf leaf tree = r_lookup_node (\(RNode _ _ n_leaf _) -> n_leaf == Just leaf) tree
  
r_remove_leaf :: Eq v => RTree v -> RLeaf v -> RTree v
r_remove_leaf tree leaf = r_clear_empty_nodes (new_node : ancestors)
    where node : ancestors = r_lookup_leaf leaf tree
          RNode n_elements n_zone n_leaf n_childs = node
          new_node = RNode (n_elements - 1) n_zone Nothing n_childs -- TODO: this would be a great place to reduce the zone
          r_clear_empty_nodes [] = r_void 
          r_clear_empty_nodes (RNode 0 n_zone Nothing [] : RNode p_elements p_zone p_leaf p_childs : nodes)  = r_void

-- lookup any node that matches the given node that is in the given tree; return all of its ancestors, with the tree's root as the head
r_lookup_node :: Eq v => (RTree v -> Bool) -> RTree v -> [RTree v]
r_lookup_node matcher tree
  | matcher tree = [tree]
  | length matches_from_childs > 0 = tree : head matches_from_childs -- we're using head arbitrarily; any of the child matches would work here
  | otherwise = []
  where matches_from_childs = filter (\results -> length results > 0) $ map (r_lookup_node matcher) t_childs
        (RNode t_num_elements t_zone t_leaf t_childs) = tree

r_replace_node :: Eq v => (RTree v -> RTree v) -> (RTree v -> Bool) -> RTree v -> Maybe (RTree v)
r_replace_node replace match tree
  | match tree = Just $ replace tree
  | length matches_from_childs > 0 = Just $ head matches_from_childs
  | otherwise = Nothing
  where (RNode t_num_elements t_zone t_leaf t_childs) = tree
        extracted_childs = extract_each t_childs
        matches_from_childs = catMaybes $ map match_extracted_child extracted_childs
        match_extracted_child (child, others) = case r_replace_node replace match child of
                                                     Just new_child -> Just $ r_node_add_child new_child (RNode (t_num_elements - 1) t_zone t_leaf others)
                                                     Nothing -> Nothing

r_supplant_nodes :: Eq v => Bool -> Zone -> (RTree v -> Bool) -> (RTree v -> RTree v) -> RTree v -> (RTree v, Bool)
r_supplant_nodes supplant_all zone match replace tree
  | zone_overlaps zone t_zone = foldr try_supplant_child (try_supplant_leaf tree) childs_with_neighbors
  | otherwise = (tree, False)
    where (RNode t_num_elements t_zone t_leaf t_childs) = tree
          --try_supplant_leaf :: Eq v => RTree v -> (RTree v, Bool)
          try_supplant_leaf tree = if match tree then (replace tree, True) else (tree, False)
          childs_with_neighbors = extract_each t_childs
          --try_supplant_child :: Eq v => (RTree v, [RTree v]) -> (RTree v, Bool) -> (RTree v, Bool)
          try_supplant_child (child, other_childs) (parent, parent_has_changed) =
            if (supplant_all || not parent_has_changed) && child_has_changed -- if we're supplanting all OR a change hasn't been made yet, and ALSO if this child changed, then do the replace
                then (r_node_add_child new_child (RNode (p_num_elements - c_num_elements) p_zone p_leaf other_childs), True)
                else (parent, parent_has_changed)
            where (new_child, child_has_changed) = r_supplant_nodes supplant_all zone match replace child
                  (RNode p_num_elements p_zone p_leaf _) = parent
                  (RNode c_num_elements _ _ _) = child
                  
-- delete all instances of the given leaf
r_delete_leafs :: Eq v => Bool -> RLeaf v -> RTree v -> RTree v
r_delete_leafs delete_all leaf tree = fst $ r_supplant_nodes delete_all (pos_to_zone l_pos) (\(RNode _ _ n_leaf _) -> Just leaf == n_leaf) (\_ -> r_void) tree
    where (RLeaf l_pos _) = leaf

r_delete_leaf :: Eq v => RLeaf v -> RTree v -> RTree v
r_delete_leaf = r_delete_leafs False

-- delete all leafs in the given zone
r_delete_zone :: Eq v => Zone -> RTree v -> RTree v
r_delete_zone zone tree = fst $ r_supplant_nodes True zone (\_ -> True) (\_ -> r_void) tree

{-                                                     
-- foreach matching child
-- r_replace_node on it
-- if it's not Nothing, then remove and reinsert it
r_replace_nodes :: Eq v => (RTree v -> RTree v) -> (RTree v -> Bool) -> RTree v -> Maybe (RTree v)
r_replace_nodes replace match tree
  | has_match = Just new_tree
  | otherwise = Nothing
  where (RNode t_num_elements t_zone t_leaf t_childs) = tree
        extracted_childs_with_match = catMaybes $ map match_extracted_child (extract_each t_childs)
        new_tree = foldr replace_extracted_child tree_with_leaf_replaced extracted_childs_with_match
        (tree_with_leaf_replaced, has_match) = if match tree then (replace tree, True) else (tree, length extracted_childs_with_match > 0)
        match_extracted_child (child, others) = case r_replace_node replace match child of
                                                     Just new_child -> Just (new_child, others)
                                                     Nothing -> Nothing
        replace_extracted_child (new_child, others) node = r_node_add_child new_child (n_num_elements - 1) n_zone n_leaf others
            where (RNode n_num_elements n_zone n_leaf _) = node

r_delete_leaf :: Eq v => RLeaf v -> RTree v -> RTree v
r_delete_leaf leaf tree = case r_replace_node (\_ -> r_void) (\(RNode _ _ n_leaf _) -> n_leaf == Just leaf) tree of
                              Just result -> result
                              Nothing -> tree
-}

test_this_module = do
    let leaves = [
                    ([3,5], "garbage"),
                    ([9,4], "mental"),
                    ([1,5], "fifteen"),
                    ([0,0], "origin"),
                    ([3,3], "third")
                 ]

    let peeves = map (\n -> ([n,n], (n+n))) [1..9] :: [(Pos, Int)]

    let trees = foldr (\(pos, value) trees -> r_insert 3 (RLeaf pos value) (head trees) : trees) [r_void] leaves

    --mapM_ print trees
    mapM_ putStrLn $ map (r_print 0) trees

