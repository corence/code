
module RangeSolver
(
) where

import qualified Zone
import Zone (Zone(..), Pos)
import qualified TRTree
import TRTree (Tree(..))

-- We have the following data structures:
-- 1) The grid. Each Pos has exactly one cell. Each cell has a Pos and a state -- liquid (uncertain) by default.
-- 2) The hints. Each hint has:
--   - a horz zone
--   - a vert zone
--   - a value
-- 3) The hints are stored as Clear or Dirty hints.
-- 4) Changing a cell dirties all the hints that govern that cell.

data CellState = Solid | Liquid | Gas
type Grid = Tree CellState
data Cell = Cell Pos CellState
data Hint = Hint Int (Tree Pos) (Tree Pos) -- value, horz_zone, vert_zone
data SolverState = SolverState Grid [Hint] [Hint] -- cells, clear, dirty

init_grid :: Int -> Int -> Grid

create_hint :: Int -> Pos -> Hint

init_hint :: Hint -> SolverState -> SolverState

solve :: SolverState -> Maybe SolverState
solve (SolverState grid [] [] resolved) = Nothing
solve (SolverState grid [] (hint:potential) resolved) = case find_hint_value grid hint of
                                                             Nothing -> solve $ SolverState grid [] potential (hint:resolved)
                                                             Just replacement_cell -> set_cell_in_state replacement_cell (SolverState grid [] (hint:potential) resolved)
solve (SolverState grid (hint:outdated) potential resolved) = case update_hint grid hint of
                                                                   Nothing -> solve $ SolverState grid outdated (hint:potential) resolved
                                                                   Just replacement_hint -> SolverState grid outdated (replacement_hint:potential) resolved

find_hint_value :: Grid -> Hint -> (Maybe Hint, Maybe Cell)
find_hint_value grid hint = find_hint_value_x grid (choose_first_liquids grid hint)

find_hint_value_x :: Grid -> [Hint] -> [Cell] -> Maybe Cell
find_hint_value_x _ _ [] = Nothing
find_hint_value_x grid hints (cell:cells)

-- solve algorithm:
-- 1) if there are no dirty hints, we are Done (succeeded or failed)
-- 2) pick any dirty hint (arbitrarily)
--     2a) reduce the hint's horz and vert sizes (if there are any solids in them)
--     2b) pick each first liquid cell in each of west/north/east/south (up to 4 of these)
--         2a1) if there are no liquid cells in it, then destroy this hint
--         2a2) for each of these liquids, try setting it to Solid and then to Gas
--             2a2a) when changing this cell, lookup every dirty and clear hint that governs this cell
--             2a2b) if any of these hints are contradicted, the Liquid state is disproven and we return the new value
--     2c) if a new value for one of these liquids was picked, return it (along with the set of hints that will now be dirty?)
--     2d) if none were found, clear the dirt on this hint
