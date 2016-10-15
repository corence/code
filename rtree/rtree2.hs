
import GHC.Exts

-- insert into void: create leaf
-- insert into nonfull leaf: extend and add to that leaf
-- insert into full leaf: replace the leaf with a node containing the old leaf and a new leaf containing the value
-- insert into node, where num_elements < capacity * height: 

-- "nonfull": num_elements < capacity * height
-- insert into nonfull node, with all full children: 

-- insert into node:
--  0) definition of "full": (num_elements >= capacity * height)
--  1) is this node full? if so, spawn a new parent node for this one, and spawn a new leaf node that belongs to it too
--  2) sort the child nodes by their num_elements
--  3) *then* sort them by whether or not they contain the new pos
--  4) if sorted_childs is empty, and num_childs >= capacity, spawn a new parent for this one
--  5) if sorted_childs is empty, spawn a new leaf child of this one
--  6) take the first sorted_childs and insert into that. then, replace that value in childs.

type Pos = [Int]
data Zone = ZVoid | Zone Pos Pos deriving Show

zone_contains :: Zone -> Pos -> Bool
zone_contains ZVoid _ = False
zone_contains (Zone nw se) pos = and (zipWith (<=) nw pos) && and (zipWith (>=) se pos)

zone_extend :: Zone -> Pos -> Zone
zone_extend ZVoid pos = Zone pos pos
zone_extend (Zone nw se) pos = Zone most_nw most_se
  where most_nw = zipWith min nw pos
        most_se = zipWith max se pos

zone_add :: Zone -> Zone -> Zone
zone_add ZVoid zone = zone
zone_add zone ZVoid = zone
zone_add zone (Zone nw se) = zone_extend (zone_extend zone nw) se

-- RNode num_elements height zone childs | RLeaf height pos value
data RTree v = RNode Int Int Zone [RTree v] | RLeaf Int Pos v deriving Show
r_void = RNode 0 1 ZVoid []

r_num_elements (RLeaf _ _ _) = 1
r_num_elements (RNode num_elements _ _ _) = num_elements

r_height (RLeaf height _ _) = height
r_height (RNode _ height _ _) = height

r_zone (RLeaf _ pos _) = Zone pos pos
r_zone (RNode _ _ zone _) = zone

r_childs (RLeaf _ _ _) = []
r_childs (RNode _ _ _ childs) = childs

r_generate_new_parent :: RTree v -> Pos -> v -> RTree v
r_generate_new_parent tree pos value = RNode (1 + r_num_elements tree) (1 + r_height tree) (zone_extend (r_zone tree) pos) [RLeaf (r_height tree) pos value, tree]



--show :: RTree v -> String
--show (RNode num_elements height (Zone nw se) childs) = "{" ++ show num_elements ++ ", " ++ show height ++ ", <" ++ show nw ++ "-" ++ show se ++ ">, " ++ show childs
--show (RLeaf height pos value) = "(" ++ show height ++ ", " ++ show pos ++ ", " ++ show value ++ ")"

-- r_add_child node child
r_add_child :: RTree v -> RTree v -> RTree v
r_add_child node child = RNode (n_elements + c_elements) (max n_height (1 + c_height)) (zone_add n_zone c_zone) (child : n_childs)
    where (RNode n_elements n_height n_zone n_childs) = node
          (RNode c_elements c_height c_zone c_childs) = child

-- r_insert capacity node pos value
r_insert :: Int -> RTree v -> Pos -> v -> RTree v
r_insert _ (RLeaf lHeight lPos lValue) pos value = r_generate_new_parent (RLeaf lHeight lPos lValue) pos value
r_insert capacity node pos value
  | not (can_take_more node) = r_generate_new_parent node pos value
  | length childs_containing_pos > 0 && can_take_more ccp = r_add_child (node_with_child_removed ccps) (r_insert capacity ccp pos value)
  | length childs > 0 && can_take_more c = r_add_child (node_with_child_removed cs) (r_insert capacity c pos value)
  | length childs < capacity = r_add_child node (RLeaf (r_height node - 1) pos value)
  | otherwise = error "im not really sure where to insert anymore"
  where childs = sortWith r_num_elements $ r_childs node
        (c:cs) = childs
        childs_containing_pos = filter (\x -> zone_contains (r_zone x) pos) childs
        (ccp:ccps) = childs_containing_pos
        can_take_more x = r_num_elements x < capacity * r_height x
        node_with_child_removed remaining_children = RNode (r_num_elements node - 1) (r_height node) (r_zone node) remaining_children

main = do
    let r1 = r_void
    let r2 = r_insert 3 r1 [3,5] "garbage"
    print r2
    let r3 = r_insert 3 r2 [9,4] "mental"
    print r3
    let r4 = r_insert 3 r3 [1,5] "fifteen"
    print r4
    let r5 = r_insert 3 r4 [0,0] "origin"
    print r5
    let r6 = r_insert 3 r5 [3,3] "third"
    print r6
    
    return ()

