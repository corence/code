
import qualified Zone
import Zone (Zone(..), Pos)
import qualified TRTree
import TRTree (Tree(..))

import Data.List

assertEqual :: Eq t => (t -> String) -> String -> t -> t -> IO ()
assertEqual present test_id expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ "**fail**\n" ++ test_id ++ ":\n[\n" ++ present expected ++ "\n]\n!=\n[\n" ++ present actual ++ "\n]\n"

assert0 :: (Eq v, Show v) => String -> Tree v -> Tree v -> IO ()
assert0 = assertEqual $ TRTree.print 0

main = do
    assert0 "insert single leaf into empty tree"
            (Leaf (Zone [3,4] [4,9]) "peaches")
            (TRTree.insert (Leaf (Zone [3,4] [4,9]) "peaches") TRTree.empty)

    assert0 "insert leaf into another leaf"
            (Node (Zone [1,2] [9,8]) [
                Leaf (Zone [1,6] [3,8]) "strawberries",
                Leaf (Zone [4,2] [9,6]) "oranges"
                ])
            (TRTree.insert
                (Leaf (Zone [1,6] [3,8]) "strawberries")
                (Leaf (Zone [4,2] [9,6]) "oranges")
                )

    --assert0 "should split after every 3+1th insertion"

    --ascending_leafs = 
