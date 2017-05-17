
import Data.Maybe

data TreeNode a = EmptyTree | TreeNode a (TreeNode a) (TreeNode a) | InvalidTree
  deriving (Eq, Show)

tree1 :: (TreeNode Int)
tree1 = TreeNode 3 (TreeNode 1 EmptyTree (TreeNode 2 EmptyTree EmptyTree)) (TreeNode 6 EmptyTree EmptyTree)

traverseTree :: (TreeNode a) -> [a] -> [a]
traverseTree EmptyTree list = list
traverseTree InvalidTree _ = []
traverseTree (TreeNode value lhs rhs) list = traverseTree lhs (value:(traverseTree rhs list))

makeTree [] = EmptyTree
makeTree (value:values) = treeAdd (makeTree values) value

treeAdd tree value
  | tree == EmptyTree = TreeNode value EmptyTree EmptyTree
  | tree == InvalidTree = InvalidTree
  | value < treeValue = TreeNode treeValue (treeAdd left value) right
  | value > treeValue = TreeNode treeValue left (treeAdd right value)
  | otherwise = InvalidTree
  where (TreeNode treeValue left right) = tree

treeValue :: Ord a => TreeNode a -> Maybe a
treeValue node
  | node == EmptyTree = Nothing
  | node == InvalidTree = (Nothing :: Maybe a)
  | otherwise = Just value
  where (TreeNode value _ _) = node

is_nonzero num = num > 0

printTree input = do
  let tree = makeTree input :: (TreeNode Int)
  putStrLn $ (show tree) ++ ": " ++ (show $ traverseTree tree [])

--dumpTree (TreeNode value left right) indent = 

main = do
  let val = traverseTree tree1 []
  print val
  let tree2 = makeTree [5, 3, 9, 1, 8, 4, 22, 11]
  let val2 = (traverseTree tree2 [] :: [Int])
  print val2

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
                  [1, 3],
                  [-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8],
                  [3, 5, 3]
               ]
  
  mapM_ printTree inputs
