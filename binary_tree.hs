
import Data.Maybe

data (Ord a) => TreeNode a = EmptyTree | TreeNode a (TreeNode a) (TreeNode a) | InvalidTree
  deriving (Show)

tree1 :: (TreeNode Int)
tree1 = TreeNode 3 (TreeNode 1 EmptyTree (TreeNode 2 EmptyTree EmptyTree)) (TreeNode 6 EmptyTree EmptyTree)

traverseTree :: (TreeNode a) -> [a] -> [a]
traverseTree (TreeNode value lhs rhs) list = traverseTree lhs (value:(traverseTree rhs list))

makeTree :: [a] -> (TreeNode a)
makeTree [] = EmptyTree
makeTree (value:values)
  | head == EmptyTree = (TreeNode value EmptyTree EmptyTree)
  | treeValue head == Nothing = InvalidTree
  | value > fromJust (treeValue head) = (TreeNode value EmptyTree head)
  | value < fromJust (treeValue head) = (TreeNode value head EmptyTree)
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

treeValue :: TreeNode a -> Maybe a
treeValue node
  | node == Nothing = Nothing
  | node == InvalidTree = (Nothing :: Maybe a)
  | otherwise = Just value
  where (TreeNode value _ _) = node

main = do
  let val = traverseTree tree1 []
  print val
