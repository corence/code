
module TRTree
( Tree(..)
, get
, insert
, TRTree.print
, empty
, remove_zone
, replace_zone
, values
) where

import qualified Zone as Zone
import Zone (Zone(..), Pos)
import Util

data Tree v = Node Zone [Tree v] | Leaf Zone v deriving Eq
type ETree v = (Tree v, [Tree v])

empty = Node Zone.ZVoid []

max_childs_per_node = 3

insert :: Tree v -> Tree v -> Tree v
insert input node = case results of
    [] -> error "how in tarnation did that happen"
    x : [] -> x
    xs -> make_node xs
    where results = insert_level input node

insert_level :: Tree v -> Tree v -> [Tree v]
insert_level (Node _ []) node = [node] -- trying to insert an empty node? cool, we can do that fast :p
insert_level input node = case node of
  Leaf _ _ -> [node, input]
  Node _ _ -> insertion_results
  where Node _ n_childs = node
        (best_child, other_childs) = best_e_node_to_receive (tree_zone input) (extract_each n_childs)
        new_childs = insert_level input best_child
        combined_childs = smoosh new_childs other_childs
        --insertion_results = if length combined_childs > max_childs_per_node then split combined_childs else [make_node combined_childs]
        insertion_results = if length combined_childs > max_childs_per_node then split combined_childs else [add_new_childs new_childs node]
        add_new_childs new_childs node = foldr add_child node new_childs
        --pick_best_child remove_or_extract_it insert_into_that append_all_results_to_my_childs split_if_necessary

-- add a child directly to the given node without checking to see if it will fit.
add_child :: Tree v -> Tree v -> Tree v
add_child _ (Leaf _ _) = error "can't add_child to a leaf"
add_child new_child (Node n_zone n_childs) = Node (Zone.add (tree_zone new_child) n_zone) (new_child : n_childs)

make_node :: [Tree v] -> Tree v
make_node childs = Node zone (filter nonvoid childs)
    where zone = Zone.amalgamate (map tree_zone childs)

nonvoid :: Tree v -> Bool
nonvoid tree = case tree of
    (Node _ []) -> False
    otherwise -> True

-- given a set of childs, create 2 parents
split :: [Tree v] -> [Tree v]
split nodes = map make_node [left, right]
    where (left, right) = take_n (length nodes `div` 2) nodes

-- assumption: a Node can never be empty, it always has leafs somewhere
-- (protecting this assumption: insertion functions (insert_level and make_node) should always throw away an empty node)
best_e_node_to_receive :: Zone -> [ETree v] -> ETree v
best_e_node_to_receive zone nodes = head $ sortWith (\(node, _) -> Zone.expansion_required (tree_zone node) zone) nodes

tree_zone :: Tree v -> Zone
tree_zone (Node zone _) = zone
tree_zone (Leaf zone _) = zone

leaf_value :: Tree v -> v
leaf_value (Node _ _) = error "nope... nodes don't have values"
leaf_value (Leaf _ value) = value

replace_zone :: Zone -> (Tree v -> Tree v) -> Tree v -> Tree v
replace_zone zone replace tree = case tree of
    Leaf l_zone l_value -> if Zone.overlaps l_zone zone then (replace tree) else tree
    Node n_zone n_childs -> if Zone.overlaps n_zone zone then make_node (map (replace_zone zone replace) n_childs) else tree

remove_zone :: Zone -> Tree v -> Tree v
remove_zone zone tree = replace_zone zone (\tree -> empty) tree

get_leafs :: Zone -> Tree v -> [Tree v]
get_leafs zone tree = case tree of
    Leaf l_zone l_value -> if Zone.overlaps l_zone zone then [tree] else []
    Node n_zone n_childs -> if Zone.overlaps n_zone zone then concat (map (get_leafs zone) n_childs) else []

get0 :: Zone -> Tree v -> [v]
get0 zone tree = case tree of
    Leaf l_zone l_value -> if Zone.overlaps l_zone zone then [l_value] else []
    Node n_zone n_childs -> if Zone.overlaps n_zone zone then concat (map (get0 zone) n_childs) else []

get :: Zone -> Tree v -> [v]
get zone tree = map leaf_value (get_leafs zone tree)

values :: Tree v -> [v]
values tree = get (tree_zone tree) tree

print :: Int -> Tree v -> String
print depth tree = concat (replicate depth "  ")
                        ++ "<" ++ show (tree_zone tree) ++ "> "
                        ++ case tree of
                                Node _ childs -> concat (map (TRTree.print (depth + 1)) childs)
                                Leaf _ value -> "leaf"
