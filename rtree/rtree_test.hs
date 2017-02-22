
import qualified Zone
import Zone (Zone(..), Pos)
import qualified RTree
import RTree (RTree(..), RLeaf(..))
import Data.List
import System.Random

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
trees = foldr (\leaf trees -> RTree.insert 3 leaf (head trees) : trees) [RTree.void]
ctrees = trees cleaves
strees = trees sleaves

uncompiled_nodes = map (\leaf -> RTree.set_leaf leaf RTree.void) cleaves

--sequenced_nodes = map (\leaf -> RTree.insert 3 leaf (last sequenced_nodes)) cleaves

assertEqual :: (Eq t, Show t) => (t -> String) -> String -> t -> t -> IO ()
assertEqual present test_id expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ "**fail**\n" ++ test_id ++ ":\n[\n" ++ present expected ++ "\n]\n!=\n[\n" ++ present actual ++ "\n]\n"

--assert0 :: (Eq t, Show t) => String -> t -> t -> IO ()
--assert0 :: (Eq v, Show v) => String -> RTree v -> RTree v -> IO ()
assert0 = assertEqual $ RTree.print 0

assert1 :: String -> [[RTree String]] -> [[RTree String]] -> IO ()
assert1 test_id expected actual = assertEqual print_iterators test_id expected actual

print_iterators :: (Show v) => [[RTree v]] -> String
print_iterators iterators = "****************************************\n" ++ concat (map print_iterator iterators) ++ "****************************************\n"

print_iterator :: (Show v) => [RTree v] -> String
print_iterator iterator = "========================================\n" ++ concat (map print_iterator_step iterator) ++ "========================================\n"
    where print_iterator_step node = "----------------------------------------\n" ++ RTree.print 0 node ++ "----------------------------------------\n"

asserts :: (Eq a, Show a) => String -> a -> a -> IO ()
asserts = assertEqual $ show

main = do
  mapM_ putStrLn $ map (RTree.print 0) ctrees
  putStrLn $ "just showing off:\n" ++ RTree.print 0 (head strees)
  --putStrLn $ map (\t -> RTree.print 0) (tail ctrees)
  assert0 "insert_into_child" (RNode 2 (Zone [3,4] [9,5]) (Just (cleaves !! 0)) [RNode 1 (Zone [9,4] [9,4]) (Just (cleaves !! 1)) []]) (RTree.insert 3 (cleaves !! 1) (uncompiled_nodes !! 0))
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
            (RNode 5 (Zone [0,0] [9,5]) (Just $ RLeaf [3,3] "third") [
                (RNode 1 (Zone [9,4] [9,4]) (Just $ RLeaf [9,4] "mental") []),
                (RNode 1 (Zone [0,0] [0,0]) (Just $ RLeaf [0,0] "origin") []),
                (RNode 2 (Zone [1,5] [3,5]) (Just $ RLeaf [1,5] "fifteen") [
                    (RNode 1 (Zone [3,5] [3,5]) (Just $ RLeaf [3,5] "garbage") [])
                ])
            ])
            (ctrees !! 0)
            
  assert1 "r_nodes_in_zone [3,3] [3,8]"
            [
                [(head ctrees)],
                [
                    (RNode 1 (Zone [3,5] [3,5]) (Just $ RLeaf [3,5] "garbage") []),
                    (RNode 1 (Zone [1,5] [1,5]) (Just $ RLeaf [1,5] "fifteen") []),
                    (RNode 3 (Zone [0,0] [9,4]) (Just $ RLeaf [3,3] "third") [
                        (RNode 1 (Zone [9,4] [9,4]) (Just $ RLeaf [9,4] "mental") []),
                        (RNode 1 (Zone [0,0] [0,0]) (Just $ RLeaf [0,0] "origin") [])
                    ])
                ]
            ]
            (RTree.nodes_in_zone (Zone [2,2] [6,6]) (head ctrees) [])

  g <- newStdGen
  --putStrLn $ "000000"
  --putStrLn $ RTree.print 0 mecha_tree

  assertEqual show "nearest neighbour" ["50,50"] $ map leaf_value $ RTree.nearest_neighbours 4 [50, 50] mecha_tree
    where leaf_value (RLeaf _ value) = value

mecha_poses :: [(Int, Int)]
mecha_poses = zip (take 100 $ randomRs (10, 99) (mkStdGen 42)) (take 100 $ randomRs (10, 99) (mkStdGen 88))

range step min max
  | max < min = error "can't range if max is less than min"
  | max == min = [min]
  | otherwise = min : range step (min + step) max
mecha_data = map (\(x, y) -> RLeaf [x, y] (show x ++ "," ++ show y)) mecha_poses
mecha_tree = foldr (\leaf tree -> RTree.insert 3 leaf tree) RTree.void mecha_data
mecha_tree :: RTree String

--assertEqual show "nearest neighbour" -> [
--assertEqual :: (Eq t, Show t) => (t -> String) -> String -> t -> t -> IO ()

