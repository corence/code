
-- generate steps can be:
-- a) create 3-5 new cells
-- b) move

-- a) create
-- a1) how many?
-- a2) what color?
-- a3) what orientation?
-- a4) what row/col?

-- b) move
-- b1) move what?
-- b2) move where?
-- b3) how high?



{-
create or move? [
    -- create...
    -- pick a color
    -- pick a start pos
    -- pick an end pos -- 3-5 spaces from the start
    -- generate!
] else [
    -- move...
    -- pick a cell with something in it
    -- list all possible places it could move:
    --   * swap with empty neighbor
    --   * swap with full neighbor
    --   * climb onto neighbor (if i have nothing on my head)
    -- pick one of those places
    -- move it!
]
 -}

-- creates every possible combination of 2 lists
permute_zip :: [a] -> [a] -> [(a, a)]
permute_zip list1 list2 = concat (map (\element1 -> map (\element2 -> (element1, element2)) list2) list1)

data Color = Translucent | Red | Yellow | Blue | Green | Pink | Black | Orange
data Board = Board {
    board_num_rows :: Int,
    board_num_columns :: Int,
    board_cells :: [[Color]]
}
type Action = Board -> Board
type Pos = (Int, Int)
type Line = [Pos]

type Random = Int

-- board model

-- insert into the board at the given x/y index, pushing other stuff up
board_insert :: Color -> Pos -> Board -> Board
board_insert color (0, y) (column:columns) = (column_insert color y column) : columns
board_insert color (x, y) (column:columns) = column : (board_insert color (x - 1, y) columns)

-- insert into a column at the given y index, pushing other stuff up
column_insert :: Color -> Int -> [Color] -> [Color]
column_insert color 0 cells = color : cells
column_insert color row_index (cell:cells) = cell : (column_insert color (row_index - 1) cells)

-- get a cell's color
board_cell :: Board -> Pos -> Color
board_cell board pos = Translucent -- FIXME

board_action_on_cell :: Pos -> Board -> (Board -> Board) -> Board
board_action_on_cell (0, 0) board action = action board
board_action_on_cell (0, y) board action = board_action_on_cell (0, y - 1) new_board action
    where new_board = board {

board_set_cell :: Pos -> Color -> Board -> Board
board_set_cell pos color board = 

board_swap :: Pos -> Pos -> Board -> Board
board_swap source dest = board_set_cell 

-- generator controller

generate_next_thing :: Random -> Board -> (Random, Action)
generate_next_thing random board
  | action_type == 0 = generate_next_create new_random board
  | otherwise = generate_next_move new_random board
  where (new_random, action_type) = randomize 2 random

-- generate every possible move
-- then, pick one
generate_next_move :: Random -> Board -> (Random, Action)
generate_next_move random board = random_element new_random all_moves
  where (new_random, all_moves) = generate_all_moves board

generate_all_moves :: Board -> [Action]
generate_all_moves board = concat (map (generate_all_cell_moves board) poses)
  where poses = permute_zip [0..x_max-1] [0..y_max-1]
        x_max = board_num_rows board
        y_max = board_num_columns board

generate_all_cell_moves :: Board -> Pos -> [Action]
generate_all_cell_moves board (x, y)
  | Translucent == board_cell board (x, y) = []
  | otherwise = concat (column_actions (x-1, y) ++ column_actions (x+1, y))
    where column_actions column =
            if column > 0 && column < x_max
                then board_swap ((x, y), (column, y)) : climb_for_inserts board (x, y) (column, y)
                else []
          x_max = board_num_rows board
          y_max = board_num_columns board

climb_for_inserts :: Board -> Pos -> Pos -> [Action]
climb_for_inserts board source (dest_x, dest_y)
  | Translucent == board_cell board (dest_x, dest_y) = []
  | otherwise = board_move source (dest_x, dest_y) : climb_for_inserts board source (dest_x, dest_y + 1)

create_cells :: Color -> Line -> Board -> Board
create_cells color [] board = board
create_cells color (pos:poses) board = create_cells color poses board_after_inserting_one
    where board_after_inserting_one = board_insert color pos board


