
import RTree

leaves = [
                ([3,5], "garbage"),
                ([9,4], "mental"),
                ([1,5], "fifteen"),
                ([0,0], "origin"),
                ([3,3], "third")
             ]

peeves = map (\n -> ([n,n], (n+n))) [1..9] :: [(Pos, Int)]

trees = foldr (\(pos, value) trees -> r_insert 3 (RLeaf pos value) (head trees) : trees) [r_void] leaves


uncompiled_nodes = map (\(pos, value) -> r_set_leaf (RLeaf pos value) r_void) leaves

assertEqual :: (Eq t, Show t) => String -> t -> t -> IO ()
assertEqual test_id expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ test_id ++ ": [" ++ show expected ++ "] != [" ++ show actual ++ "]"

main = do
  assertEqual "insert_into_child" (head uncompiled_nodes) r_void
