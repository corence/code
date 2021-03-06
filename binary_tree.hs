
import Data.Maybe

data TreeNode a = EmptyTree | TreeNode a (TreeNode a) (TreeNode a) | InvalidTree
  deriving (Eq, Show)

tree1 :: (TreeNode Int)
tree1 = TreeNode 3 (TreeNode 1 EmptyTree (TreeNode 2 EmptyTree EmptyTree)) (TreeNode 6 EmptyTree EmptyTree)

traverseTree :: (TreeNode a) -> [a] -> [a]
traverseTree EmptyTree list = list
traverseTree (TreeNode value lhs rhs) list = traverseTree lhs (value:(traverseTree rhs list))

makeTree :: Ord a => [a] -> (TreeNode a)
makeTree [] = EmptyTree
makeTree (value:values)
  | head == EmptyTree = (TreeNode value EmptyTree EmptyTree)
  | treeValue head == Nothing = InvalidTree
  | value < fromJust (treeValue head) = (TreeNode value EmptyTree head)
  | value > fromJust (treeValue head) = (TreeNode value head EmptyTree)
  | otherwise = InvalidTree
  where head = makeTree values

--makeTree (value:values) =
  --if (head == EmptyTree)
      --then TreeNode value EmptyTree EmptyTree
      --else if (isNothing headValue)
          --then InvalidTree
          --else if value > fromJust headValue
              --then TreeNode value Nothing head
              --else if value < fromJust headValue
                  --then TreeNode value head Nothing
                  --else InvalidTree
  --where head = makeTree values
        --headValue = treeValue head

treeValue :: Ord a => TreeNode a -> Maybe a
treeValue node
  | node == EmptyTree = Nothing
  | node == InvalidTree = (Nothing :: Maybe a)
  | otherwise = Just value
  where (TreeNode value _ _) = node

is_nonzero num = num > 0

printTree input = do
  let tree = makeTree input
  putStrLn $ (show tree) ++ ": " ++ (show $ traverseTree tree [])

main = do
  let val = traverseTree tree1 []
  print val
  let tree2 = makeTree [5, 3, 9, 1, 8, 4, 22, 11]
  let val2 = traverseTree tree2 []
  print val2

  print $ map is_nonzero [1, 2, 3]

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
  
  mapM_ printTree inputs
  
  print $ traverseTree (makeTree [1, 2, 3]) []
  print $ traverseTree (makeTree [1, 3, 2]) []
  print $ traverseTree (makeTree [2, 1, 3]) []
  print $ traverseTree (makeTree [2, 3, 1]) []
  print $ traverseTree (makeTree [3, 1, 2]) []
  print $ traverseTree (makeTree [3, 2, 1]) []
