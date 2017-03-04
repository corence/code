
module RRTree (
) where

import qualified Zone as Zone
import Zone (Zone(..), Pos)

-- concat 2 lists but i don't care what order you put them in
rconcat :: [a] -> [a] -> [a]
rconcat [] ys = ys
rconcat (x:xs) ys = rconcat xs (x : ys)

extract_each :: [a] -> [(a, [a])]
extract_each [] = []
extract_each (key:elements) = (key, elements) : (map (\(k, es) -> (k, key : es)) $ extract_each elements)

max_childs_per_node = 4
max_leafs_per_node = 6

-- all leaves float on a pond (same height)

data Tree v = Node Zone [Tree v] | Branch Zone [Leaf v]
data Leaf v = Leaf Pos v

-- the algo for insert:
-- 1) navigate through each Tree level, picking the child that will expand least
-- 2) when we reach the Branch level:
-- 2a) if there's space in the branch, add the leaf and return this branch
-- 2b) if not, split the branch into two branches and return _both_
-- 3) at each level, insert this new tree into the old tree
-- 4) also, at each level, can the extra tree fit into this one? (what criteria? probably max childs per treenode)
-- 4a) if yes, fold it into the main tree
-- 4b) if no, keep ascending with both
-- requires functions:
-- a) insert into branch :: Leaf v -> Tree v -> [Tree v]
-- b) insert into node :: Leaf v -> Tree v -> [Tree v]

-- (result, extra_branch_that_needs_to_be_stuck_somewhere)
insert :: Leaf v -> Tree v -> Tree v
insert leaf tree = case result of
                        output : [] -> output
                        otherwise -> error $ "bad result length of insert: " ++ show (length result)
    where result = insert_leaf leaf tree
    
insert_leaf :: Leaf v -> Tree v -> [Tree v]
-- insert_leaf into a node:
-- 1) extract children
-- 2) pick the best child to insert into
-- 3) insert
-- 4) is there a result?
-- 4a) yes. can a result be combined into this tree?
-- 4aa) yes. do it
-- 4ab) no. don't
insert_leaf leaf (Node zone childs) = make_tree new_childs
    where 
        extracted_childs = extract_each childs
        best_child_pair = best_child_to_insert_into (leaf_pos leaf) extracted_childs

insert2 leaf (Node zone childs) = make_tree ((insert leaf best_child) : other_childs)
  where (best_child, other_childs) = best_thing
        extracted_childs = extract_each childs
insert2 leaf (Branch zone leafs)
  | length leafs > max_leafs_per_node = ((make_branch leafs1), leafs2)
  | otherwise = Branch (Zone.extend pos zone) (leaf : leafs)
  where (Leaf pos _) = leaf
        (leafs1, leafs2) = split (leaf : leafs)

-- this is a stupid split algorithm but it fits the interface
split :: [Leaf v] -> ([Leaf v], [Leaf v])
split leafs = split_n (length leafs / 2) leafs

split_n 0 leafs = ([], leafs)
split_n n (leaf:leafs) = (leaf : left, right)
    where (left, right) = split_n (n - 1) leafs

make_tree :: [Tree v] -> Tree v
make_tree childs = Node (Zone.amalgamate $ map tree_zone childs) childs

make_branch :: [Leaf v] -> Tree v
make_branch leafs = Branch (Zone.create_container $ map leaf_pos leafs) leafs

tree_zone :: Tree v -> Zone
tree_zone (Node zone _) = zone
tree_zone (Branch zone _) = zone

leaf_pos :: Leaf v -> Pos
leaf_pos (Leaf pos _) = pos

