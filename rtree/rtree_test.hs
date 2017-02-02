
import RTree
import Data.List

crazy_data = [
                ([3,5], "garbage"),
                ([9,4], "mental"),
                ([1,5], "fifteen"),
                ([0,0], "origin"),
                ([3,3], "third")
             ]

simpler_data = map (\n -> ([(n `mod` 5) `mod` 4,(n `mod` 6) `mod` 4], n)) [1..40] :: [(Pos, Int)]

leaves = map (\(pos, value) -> RLeaf pos value)
cleaves = leaves crazy_data
sleaves = leaves simpler_data

trees :: [RLeaf v] -> [RTree v]
trees = foldr (\leaf trees -> r_insert 3 leaf (head trees) : trees) [r_void]
ctrees = trees cleaves
strees = trees sleaves

uncompiled_nodes = map (\leaf -> r_set_leaf leaf r_void) cleaves

--sequenced_nodes = map (\leaf -> r_insert 3 leaf (last sequenced_nodes)) cleaves

assertEqual :: (Eq t, Show t) => (t -> String) -> String -> t -> t -> IO ()
assertEqual present test_id expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ test_id ++ ":\n[" ++ present expected ++ "]\n!=\n[" ++ present actual ++ "]"

assert0 = assertEqual $ r_print 0

asserts :: (Eq a, Show a) => String -> a -> a -> IO ()
asserts = assertEqual $ show

main = do
  mapM_ putStrLn $ map (r_print 0) ctrees
  --putStrLn $ map (\t -> r_print 0) (tail ctrees)
  assert0 "insert_into_child" (RNode 2 (Zone [3,4] [9,5]) (Just (cleaves !! 0)) [RNode 1 (Zone [9,4] [9,4]) (Just (cleaves !! 1)) []]) (r_insert 3 (cleaves !! 1) (uncompiled_nodes !! 0))
  assert0 "insert_into_child 1 time" 
            (RNode 1 (Zone [3,3] [3,3]) (Just (cleaves !! 4)) [
            ])
            (ctrees !! 4)
  assert0 "insert_into_child 2 time" 
            (RNode 2 (Zone [0,0] [3,3]) (Just (cleaves !! 4)) [
                (RNode 1 (Zone [0,0] [0,0]) (Just (cleaves !! 3)) [])
            ])
            (ctrees !! 3)
  assert0 "insert_into_child 3 time" 
            (RNode 3 (Zone [0,0] [3,5]) (Just (cleaves !! 4)) [
                (RNode 1 (Zone [1,5] [1,5]) (Just (cleaves !! 2)) []),
                (RNode 1 (Zone [0,0] [0,0]) (Just (cleaves !! 3)) [])
            ])
            (ctrees !! 2)
  assert0 "insert_into_child 4 time" 
            (RNode 4 (Zone [0,0] [9,5]) (Just (cleaves !! 4)) [
                (RNode 1 (Zone [9,4] [9,4]) (Just (cleaves !! 1)) []),
                (RNode 1 (Zone [1,5] [1,5]) (Just (cleaves !! 2)) []),
                (RNode 1 (Zone [0,0] [0,0]) (Just (cleaves !! 3)) [])
            ])
            (ctrees !! 1)
  assert0 "insert_into_child 5 time" 
            (RNode 5 (Zone [0,0] [9,5]) (Just (cleaves !! 4)) [
                (RNode 1 (Zone [1,5] [1,5]) (Just (cleaves !! 2)) []),
                (RNode 1 (Zone [0,0] [0,0]) (Just (cleaves !! 3)) []),
                (RNode 2 (Zone [3,4] [9,5]) (Just (cleaves !! 1)) [
                    (RNode 1 (Zone [3,5] [3,5]) (Just (cleaves !! 0)) [])
                ])
            ])
            (ctrees !! 0)
  putStrLn $ "just showing off:\n" ++ r_print 0 (head strees)
  asserts "lookup all [2,1] leaves"
            [(RLeaf [2,1] 37), (RLeaf [2,1] 7), (RLeaf [2,1] 17)]
            (r_lookup_pos [2,1] (head strees))
  asserts "lookup all leaves in zone [1,1] [1,19]"
            [
                RLeaf [1,1] 1,
                RLeaf [1,1] 11,
                RLeaf [1,3] 21,
                RLeaf [1,2] 26,
                RLeaf [1,1] 31
                ]
            (sortBy (\(RLeaf _ v1) (RLeaf _ v2) -> compare v1 v2) $ r_lookup_zone (Zone [1,1] [1,19]) (head strees))

  assert0 "remove all leaves in zone [0,1] [1,19] from ctrees"
            (RNode 3 (Zone [0,0] [9,5]) (Just (cleaves !! 4)) [
                (RNode 2 (Zone [3,4] [9,5]) (Just (cleaves !! 1)) [
                    (RNode 1 (Zone [3,5] [3,5]) (Just (cleaves !! 0)) [])
                ])
            ])
            (map (r_remove_leaf (head ctrees)) (r_lookup_zone (Zone [0,0] [1,19]) (head ctrees)))
            
  assert0 "remove all leaves in zone [2,2] [6,6] from ctrees"
            (RNode 3 (Zone [0,0] [9,5]) Nothing [
                (RNode 1 (Zone [1,5] [1,5]) (Just (cleaves !! 2)) []),
                (RNode 1 (Zone [0,0] [0,0]) (Just (cleaves !! 3)) []),
                (RNode 1 (Zone [3,4] [9,5]) (Just (cleaves !! 1)) [])
            ])
            (map (r_remove_leaf (head ctrees)) (r_lookup_zone (Zone [0,1] [1,19]) (head ctrees)))
