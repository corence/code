
module Sudoku where

import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Control.Applicative

import Data.List

type CellID = String
type Group = Set CellID
type Possibilities = Set Char

data Solver = Solver {
    possibilities :: Map CellID Possibilities,
    groups :: [Group],
    dirty :: Set CellID,
    output :: Map CellID Char
    }

cutPossibilityFromCell :: Char -> CellID -> Map CellID Possibilities -> Map CellID Possibilities
cutPossibilityFromCell value cellID = Map.update (Just . Set.delete value) cellID

cutPossibility :: Char -> Map CellID Possibilities -> (Map CellID Possibilities, [CellID])
--cutPossibility value possibilities = 

cutCellFromGroup :: CellID -> Group -> Group
cutCellFromGroup = Set.delete CellID

-- union of a whole bunch of sets
concatSets :: Ord a => [Set a] -> Set a
concatSets = foldr Set.union Set.empty

setValue :: Char -> CellID -> Solver -> Solver
setValue value cellID solver =
    solver {
        possibilities = newPossibilities,
        groups = filteredGroups ++ otherGroups,
        dirty = concatSets [Set.fromList restrictedCells, filteredGroups, dirty solver],
        output = Map.insert cellID value (solver output)
        }
    where (newPossibilities, restrictedCells) = cutPossibility value (Map.delete cellID (possibilities solver))
          (groupsWithThisCell, otherGroups) = partition (Set.member cellID) (groups solver)
          filteredGroups = map (Set.delete cellID) groupsWithThisCell
