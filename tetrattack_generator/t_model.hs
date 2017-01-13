
module TetrattackBoard
( Action
, Board(..)
, Color(..)
, Pos
, board_cell
, board_insert
, board_move_and_insert
, board_swap
, no_action
) where

import Data.Map (Map)
import qualified Data.Map as Map

{-
( Puzzle (..)
, PuzzleCell
, ProtoCell
, puzzleToBoard
, puzzleUnwrapDirs
) where
-}

data Color = Red | Yellow | Blue | Green | Pink | Black | Orange
data Board = Board {
    board_num_rows :: Int,
    board_num_columns :: Int,
    board_cells :: Map Pos Color
}
type Action = Board -> Board
type Pos = (Int, Int)
data Direction = DUp | DDown | DLeft | DRight

no_action = (\board -> board)

map_unconditional_assign :: Ord k => k -> Maybe v -> Map k v -> Map k v
map_unconditional_assign key Nothing mappy = Map.delete key mappy
map_unconditional_assign key (Just value) mappy = Map.insert key value mappy

pos_direction :: Direction -> Pos -> Pos
pos_direction DUp (x, y) = (x, y + 1)
pos_direction DLeft (x, y) = (x - 1, y)
pos_direction DRight (x, y) = (x + 1, y)
pos_direction DDown (x, y) = (x, y - 1)

board_swap :: Pos -> Pos -> Board -> Board
board_swap pos1 pos2 board = board_set_cell color2 pos1 (board_set_cell color1 pos2 board)
    where color1 = board_cell pos1 board
          color2 = board_cell pos2 board

board_move_and_insert :: Pos -> Pos -> Board -> Board
board_move_and_insert source_pos dest_pos board = board_insert source_cell dest_pos (board_delete source_pos board)
    where source_cell = board_cell source_pos board

board_insert :: Maybe Color -> Pos -> Board -> Board
board_insert color pos board = board_set_cell color pos shifted_board
    where pos_above = pos_direction DUp pos
          color_above = board_cell pos_above board
          shifted_board = case color_above of
                            Nothing -> board
                            Just c -> board_insert color_above pos_above board

board_delete :: Pos -> Board -> Board
board_delete pos board = board { board_cells = Map.delete pos (board_cells board) }

board_cell :: Pos -> Board -> Maybe Color
board_cell pos board = Map.lookup pos (board_cells board)

board_set_cell :: Maybe Color -> Pos -> Board -> Board
board_set_cell color pos board = board { board_cells = map_unconditional_assign pos color (board_cells board) }
