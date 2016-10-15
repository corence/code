
--import Data.List.Ordered
import GHC.Exts

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
data RTree v = RNode Int Zone [RTree v] | RLeaf Pos v deriving Show
r_void = RNode 0 ZVoid []

r_num_elements (RLeaf _ _) = 1
r_num_elements (RNode num_elements _ _) = num_elements

r_zone (RLeaf pos _) = Zone pos pos
r_zone (RNode _ zone _) = zone

r_childs (RLeaf _ _) = []
r_childs (RNode _ _ childs) = childs

-- r_add_child child node
r_add_child :: RTree v -> RTree v -> RTree v
r_add_child child node = RNode (n_elements + c_elements) (zone_add n_zone c_zone) (child : n_childs)
    where (RNode n_elements n_zone n_childs) = node
          (RNode c_elements c_zone c_childs) = child

r_pair_nodes :: RTree v -> RTree v -> RTree v
r_pair_nodes first second = r_add_child second $ r_add_child r_void first

-- 0) remove the child
-- 1) insert into the child
-- 2) update num_elements, zones, and childs (childs must be sorted)
r_insert_into_child :: Int -> RTree v -> RTree v -> RTree v
r_insert_into_child capacity leaf node = RNode (n_elements + 1) (zone_extend l_pos n_zone) (new_child:n_other_childs)
  where (RNode n_elements n_zone n_childs) = node
        (RLeaf l_pos l_value) = leaf
        new_child = r_insert capacity n_child leaf
        (n_child:n_other_childs) = sortWith r_num_elements n_childs
--        new_childs = insertBagBy (comparing r_num_elements) new_child n_childs -- multiset isn't in the standard library :(

-- insertBagBy (a -> a -> Ordering) -> a -> [a] -> [a]

-- insert into node:
-- 0) is this node void? then just return a new leaf
-- 1) do any of the childs contain the new point? then insert into the one with least children. tiebreaker: arbitrary
-- 2) can this node take another child? add it as a new leaf
-- 3) otherwise: find the child with the rect nearest to the new point, and extend it
r_insert :: Int -> RTree v -> RTree v -> RTree v
r_insert capacity leaf node
  | length childs_containing_new_pos > 0 = r_insert_into_child capacity leaf node
  | length n_childs < capacity = r_add_child leaf node
  | otherwise = r_pair_nodes leaf node
    where (RNode n_elements n_zone n_childs) = node
          (RLeaf l_pos l_value) = leaf
          childs_containing_new_pos = sortWith r_num_elements $ filter (\c -> zone_contains l_pos (r_zone c)) n_childs

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
