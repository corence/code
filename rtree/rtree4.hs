
--import Data.List.Ordered
import GHC.Exts
import Data.Maybe

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
data Zone = ZVoid | Zone Pos Pos deriving Show

zone_contains :: Pos -> Zone -> Bool
zone_contains _ ZVoid = False
zone_contains pos (Zone nw se) = and (zipWith (<=) nw pos) && and (zipWith (>=) se pos)

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
data RTree v = RNode Int Zone (Maybe (RLeaf v)) [RTree v] deriving Show
data RLeaf v = RLeaf Pos v deriving Show
r_void = RNode 0 ZVoid Nothing []

r_set_leaf :: RLeaf v -> RTree v -> RTree v
r_set_leaf leaf node = RNode (n_elements + 1) (zone_extend l_pos n_zone) (Just leaf) n_childs
    where (RNode n_elements n_zone Nothing n_childs) = node
          (RLeaf l_pos _) = leaf

r_node_add_child :: RTree v -> RTree v -> RTree v
r_node_add_child child node = RNode (c_num_elements + n_num_elements) (zone_add c_zone n_zone) n_leaf (sorted_insert_by_num_elements child n_childs)
  where (RNode n_num_elements n_zone n_leaf n_childs) = node
        (RNode c_num_elements c_zone c_leaf c_childs) = child

r_node_remove_first_child :: RTree v -> (RTree v, RTree v)
r_node_remove_first_child node = (n_child, RNode (n_num_elements - c_num_elements) n_zone n_leaf n_childs) -- we don't *have* to reduce the zone here, but it would be nice
  where (RNode n_num_elements n_zone n_leaf (n_child:n_childs)) = node
        (RNode c_num_elements c_zone c_leaf c_childs) = n_child
        
sorted_insert_by_num_elements :: RTree v -> [RTree v] -> [RTree v]
sorted_insert_by_num_elements element [] = [element]
sorted_insert_by_num_elements element  (n:ns) = if e_num_elements < n_num_elements
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
-- 1) can this node take another child? add it as a new child, and insert into that
-- 1) does this node have a leaf? if not, add it here
-- 3) otherwise: find the child with the rect nearest to the new point, and extend it
r_insert :: Int -> RLeaf v -> RTree v -> RTree v
r_insert capacity leaf node
  | length n_childs_containing_new_pos > 0 = r_insert_into_first_child capacity leaf node
  | length n_childs < capacity = r_insert_into_first_child capacity leaf (r_node_add_child r_void node)
  | isNothing n_leaf = r_set_leaf leaf node
  | otherwise = r_insert capacity leaf (r_node_add_child node r_void)
    where (RNode n_elements n_zone n_leaf n_childs) = node
          (RLeaf l_pos _) = leaf
          n_childs_containing_new_pos = filter (\c -> zone_contains l_pos (r_node_zone c)) n_childs
          r_node_zone (RNode _ zone _ _) = zone

-- return all leaves in a zone
r_lookup_zone :: Zone -> RTree v -> [RLeaf v]
r_lookup_zone zone (RNode n_num_elements n_zone n_leaf n_childs) = case matching_leaf of
                                                                       Just leaf -> leaf : child_results
                                                                       Nothing -> child_results
    where child_results = if zone_overlaps x y
                              then concat $ map (r_lookup_zone zone) n_childs
                              else nothing
          matching_leaf = case n_leaf of
                              Just (RLeaf l_pos l_value) -> if zone_contains l_pos zone
                                                                then Just (RLeaf l_pos l_value)
                                                                else Nothing
                              Nothing -> Nothing


main = do
    let leaves = [
                    ([3,5], "garbage"),
                    ([9,4], "mental"),
                    ([1,5], "fifteen"),
                    ([0,0], "origin"),
                    ([3,3], "third")
                 ]

    let trees = foldr (\(pos, value) trees -> r_insert 3 (RLeaf pos value) (head trees) : trees) [r_void] leaves

    mapM_ print trees

