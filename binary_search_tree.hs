
import Data.Maybe

data TreeNode a = EmptyTree | TreeNode a (TreeNode a) (TreeNode a)
  deriving (Eq, Show)

instance Functor TreeNode where
    fmap _ EmptyTree = EmptyTree
    fmap func (TreeNode value left right) = TreeNode (func value) (fmap func left) (fmap func right)

tree1 :: (TreeNode Int)
tree1 = TreeNode 3 (TreeNode 1 EmptyTree (TreeNode 2 EmptyTree EmptyTree)) (TreeNode 6 EmptyTree EmptyTree)

traverseTree :: (TreeNode a) -> [a] -> [a]
traverseTree EmptyTree list = list
traverseTree (TreeNode value lhs rhs) list = traverseTree lhs (value:(traverseTree rhs list))

add :: Ord a => a -> TreeNode a -> TreeNode a
add value EmptyTree = TreeNode value EmptyTree EmptyTree
add value (TreeNode x left right)
  | value < x = TreeNode x (add value left) right
  | value > x = TreeNode x left (add value right)
  | otherwise = TreeNode value left right

makeTree :: Ord a => [a] -> TreeNode a
makeTree = foldr (\value tree -> add value tree) EmptyTree

get :: Ord a => a -> TreeNode a -> Maybe a
get _ EmptyTree = Nothing
get target (TreeNode value left right)
  | value < target = get target left
  | value > target = get target right
  | value == target = Just value

printTree input = do
  let tree = makeTree input
  putStrLn $ (show tree) ++ ": " ++ (show $ traverseTree tree [])

main = do
  let val = traverseTree tree1 []
  print val
  let tree2 = makeTree [5, 3, 9, 1, 8, 4, 22, 11]
  let val2 = traverseTree tree2 []
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
  
  mapM_ printTree inputs
  
  print $ traverseTree (makeTree [1, 2, 3]) []
  print $ traverseTree (makeTree [1, 3, 2]) []
  print $ traverseTree (makeTree [2, 1, 3]) []
  print $ traverseTree (makeTree [2, 3, 1]) []
  print $ traverseTree (makeTree [3, 1, 2]) []
  print $ traverseTree (makeTree [3, 2, 1]) []
