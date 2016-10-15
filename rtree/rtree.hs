
type Pos dimension = [dimension]

data Zone dimension = ZVoid | Zone [dimension] [dimension]
zone_containing :: Ord dimension => [dimension] -> Zone dimension
zone_containing pos = zone_extend ZVoid pos

zone_extend :: Ord p => Zone p -> [p] -> Zone p
zone_extend ZVoid pos = Zone pos pos
zone_extend (Zone nw se) pos = Zone most_nw most_se
  where most_nw = zipWith min nw pos
        most_se = zipWith max se pos

data RTree dimension valueType = RVoid | RNode (Zone dimension) [RTree dimension valueType] | RLeaf (Zone dimension) [([dimension], valueType)]

r_insert :: Ord dimension => Int -> RTree dimension valueType -> [dimension] -> valueType -> RTree dimension valueType
r_insert _ RVoid pos value = RLeaf (zone_extend ZVoid pos) [(pos, value)]
r_insert capacity (RLeaf zone values) pos value
  | length values < capacity = RLeaf (zone_extend zone pos) ((pos, value) : values)
  | otherwise = RNode (zone_extend zone pos) [(r_insert capacity RVoid pos value), (RLeaf zone values)]
r_insert capacity (RNode zone (subtree:subtrees)) pos value

main = do
    let r = RVoid
    let r2 = r_insert 3 r [3,5] "garbage"
    return ()
