
module TetrattackGenerator (
) where
import TetrattackBoard
import System.Random
import Data.Maybe

type Line = [Pos]

-- creates every possible combination of 2 lists
permute_zip :: [a] -> [a] -> [(a, a)]
permute_zip list1 list2 = concat (map (\element1 -> map (\element2 -> (element1, element2)) list2) list1)

-- my random until i can figure out proper syntax for this thing
type Randoms = ([Bool], [Int])

random_bool :: Randoms -> (Randoms, Bool)
random_bool (bool:bools, ints) = ((bools, ints), bool)

random_int :: Randoms -> (Randoms, Int)
random_int (bools, int:ints) = ((bools, ints), int)

random_int_range :: Randoms -> Int -> Int -> (Randoms, Int)
random_int_range randoms min max = (new_randoms, (int `mod` (max - min + 1)) + min)
    where (new_randoms, int) = random_int randoms

random_element :: Randoms -> [a] -> (Randoms, a)
random_element randoms elements = (new_randoms, elements !! index)
    where (new_randoms, index) = random_int_range randoms 0 ((length elements) - 1)
    
generate_next_thing :: Randoms -> Board -> (Randoms, Action)
generate_next_thing randoms board
  | action_type == 0 = generate_next_create new_randoms board
  | otherwise = generate_next_move new_randoms board
  where (new_randoms, action_type) = random_bool randoms

line_is_supported :: Board -> Line -> Bool
line_is_supported board [] = True
line_is_supported board ((x,y):line) = pos_is_supported && line_is_supported (board_insert (Just Red) (x,y) board) line
    where pos_is_supported = y == 0 || isJust (board_cell (x,y-1) board)

-- generate a new line
--  horz or vert
--  position
--  color
--  length (todo: combos?)
-- make sure it's valid -- don't create a horz floating in space
-- generate a whole ton of them and pick the first good one
generate_next_create :: Randoms -> Board -> (Randoms, Action)
generate_next_create randoms board = valid_candidate randoms
    where (randoms2, color) = random_element randoms [Red, Yellow, Blue, Green, Pink, Black, Orange]
          valid_candidate randoms = if (line_is_supported board line)
                                        then (randoms2, create_cells color line)
                                        else valid_candidate randoms2
            where (randoms2, line) = candidate randoms
          candidate randoms = (randoms5, line)
            where (randoms2, length) = random_int_range randoms 3 5
                  (randoms3, x) = random_int_range randoms2 0 ((board_num_columns board) - length)
                  (randoms4, y) = random_int_range randoms3 0 ((board_num_rows board) - length)
                  (randoms5, is_vert) = random_bool randoms4
                  line = if is_vert
                            then generate_line (\(x, y) -> (x, y + 1)) color length (x,y)
                            else generate_line (\(x, y) -> (x + 1, y)) color length (x,y)

generate_line :: (Pos -> Pos) -> Color -> Int -> Pos -> Line
generate_line_vert _ _ 0 _ = []
generate_line next_func color length pos = pos : generate_line next_func color (length - 1) (next_func pos)

-- generate every possible move
-- then, pick one
generate_next_move :: Randoms -> Board -> (Randoms, Action)
generate_next_move random board = random_element new_random all_moves
  where (new_random, all_moves) = generate_all_moves board

generate_all_moves :: Board -> [Action]
generate_all_moves board = concat (map (generate_all_cell_moves board) poses)
  where poses = permute_zip [0..x_max-1] [0..y_max-1]
        x_max = board_num_rows board
        y_max = board_num_columns board

generate_all_cell_moves :: Board -> Pos -> [Action]
generate_all_cell_moves board (x, y)
  | Nothing == board_cell (x, y) board = []
  | otherwise = concat (column_actions (x-1, y) ++ column_actions (x+1, y))
    where column_actions column =
            if column > 0 && column < x_max
                then board_swap ((x, y), (column, y)) : ((\board -> climb_for_inserts board (x, y) (column, y)) board)
                else []
          x_max = board_num_rows board
          y_max = board_num_columns board

climb_for_inserts :: Board -> Pos -> Pos -> [Action]
climb_for_inserts board source (dest_x, dest_y)
  | Nothing == board_cell (dest_x, dest_y) board = []
  | otherwise = board_move_and_insert source (dest_x, dest_y) : climb_for_inserts board source (dest_x, dest_y + 1)

create_cells :: Color -> Line -> Action
create_cells color [] = no_action
create_cells color (pos:poses) = (\board -> create_cells color poses (board_after_inserting_one board))
    where board_after_inserting_one board = board_insert (Just color) pos board


