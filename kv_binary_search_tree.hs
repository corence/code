
import Data.Maybe

data TreeNode key value = EmptyTree | TreeNode key value (TreeNode key value) (TreeNode key value)
  deriving (Eq, Show)

instance Functor (TreeNode key) where
    fmap func EmptyTree = EmptyTree
    fmap func (TreeNode key value left right) = TreeNode key (func value) (fmap func left) (fmap func right)

tree1 :: (TreeNode Int Int)
tree1 = TreeNode 3 4 (TreeNode 1 2 EmptyTree (TreeNode 2 6 EmptyTree EmptyTree)) (TreeNode 6 9 EmptyTree EmptyTree)

entries :: (TreeNode key value) -> [(key, value)]
entries EmptyTree = []
entries (TreeNode key value lhs rhs) = entries lhs ++ [(key, value)] ++ entries rhs

add :: Ord key => key -> value -> TreeNode key value -> TreeNode key value
add key value EmptyTree = TreeNode key value EmptyTree EmptyTree
add key value (TreeNode tKey tValue left right) = case compare key tKey of
  LT -> TreeNode tKey tValue (add key value left) right
  GT -> TreeNode tKey tValue left (add key value right)
  EQ -> TreeNode key value left right

makeTree :: Ord key => [(key, value)] -> TreeNode key value
makeTree = foldr (\(key, value) tree -> add key value tree) EmptyTree

get :: Ord key => key -> TreeNode key value -> Maybe value
get _ EmptyTree = Nothing
get target (TreeNode key value left right)
  | key < target = get target left
  | key > target = get target right
  | key == target = Just value

printTree input = do
  let tree = makeTreeX input
  putStrLn $ dumpTree (" {", ",", "}") tree ++ ": " ++ (show $ entries tree)

dumpTreeLn :: (Show key, Show value) => String -> TreeNode key value -> String
dumpTreeLn prefix EmptyTree = prefix ++ "<>\n"
dumpTreeLn prefix (TreeNode key value left right) = prefix ++ show key ++ show value ++ "\n" ++ dumpTreeLn ("  " ++ prefix) left ++ dumpTreeLn ("  " ++ prefix) right

dumpTree :: (Show key, Show value) => (String, String, String) -> TreeNode key value -> String
dumpTree _ EmptyTree = ""
dumpTree fixes (TreeNode key value left right) = prefix ++ show key ++ inner ++ show value ++ dumpTree fixes left ++ dumpTree fixes right ++ suffix
    where (prefix, inner, suffix) = fixes

main = do
  let val = entries tree1
  print val
  let tree2 = makeTreeX [5, 3, 9, 1, 8, 4, 22, 11]
  let val2 = entries tree2
  print val2

  print $ map (> 0) [1, 2, 3]

  mapM_ print [1,2,3]
  
  let inputs = [ 
                  [1, 2, 3],
                  [1, 3, 2],
                  [2, 1, 3],
                  [2, 3, 1],
                  [3, 1, 2],
                  [3, 2, 1],
                  [2],
                  [2, 1],
                  [1],
                  [1, 3],
                  [2, 1, 3],
                  [2],
                  [2, 1],
                  [1],
                  [1, 3]
               ]
  
  putStrLn "---"
  mapM_ printTree inputs
  
  print $ entries (makeTreeX [1, 2, 3])
  print $ entries (makeTreeX [1, 3, 2])
  print $ entries (makeTreeX [2, 1, 3])
  print $ entries (makeTreeX [2, 3, 1])
  print $ entries (makeTreeX [3, 1, 2])
  print $ entries (makeTreeX [3, 2, 1])

makeTreeX nums = makeTree $ map (\n -> (n, n)) nums
