
module Sudoku where

import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Control.Applicative

import Data.List

type CellID = String
type Group = Set CellID

-- A board has:
--  - map from cellID to a list of possible values
--  - then groups of CellIDs (rows, columns, squares)
--  - then a list of which CellIDs are still dirty
data Board = Board {
    possibilities :: Map CellID Possibilities,
    groups :: [Group],
    dirty :: Set CellID,
    result :: Map CellID Int
    } deriving (Show)

type Possibilities = [Char]

createRow :: Char -> [CellID]
createRow y = do
    x <- ['1'..'9']
    let cellID = x : y : []
    pure cellID

createGroup :: [Char] -> [Char] -> Group
createGroup xs ys = Set.fromList cellIDs
    where cellIDs = liftA2 (\x y -> x : y : []) xs ys

createRows :: [Group]
createRows = foldr (\y result -> createGroup ['1'..'9'] [y] : result) [] ['1'..'9']

createCols :: [Group]
createCols = foldr (\x result -> createGroup [x] ['1'..'9'] : result) [] ['1'..'9']

createSquares :: [Group]
createSquares = map (\pair -> createGroup (fst pair) (snd pair)) xyCombos
    where xyCombos = liftA2 (,) clusters clusters
          clusters = [['1','2','3'], ['4','5','6'], ['7','8','9']]

allCellIDs :: Group
allCellIDs = createGroup ['1'..'9'] ['1'..'9']

createPossibilities :: Map CellID Possibilities
createPossibilities = Map.fromList cells
    where cells = map (\cellID -> (cellID, ['1'..'9'])) (Set.toList allCellIDs)

createBoard :: Board
createBoard = Board {
    possibilities = createPossibilities,
    groups = createRows ++ createCols ++ createSquares,
    dirty = allCellIDs,
    result = Map.empty
    }

cutPossibility :: Char -> CellID -> Map CellID Possibilities -> Map CellID Possibilities
cutPossibility value ownerID = Map.update (Just . filter (/= value)) ownerID

setCell :: CellID -> Char -> Board -> Board
setCell cellID value board =
    board {
        possibilities = Map.delete cellID (possibilities board),
        groups = filteredGroups ++ otherGroups,
        dirty = Set.union touchedCellIDs (dirty board),
        result = Map.insert cellID value (board result)
        }
    -- 1) if the new value is a char that isn't in the possibilities, abort
    -- 2) remove the cell from possibilities
    -- 3) remove the cell from groups
    -- 4) each cell in each of the groups should have the value removed from their possibilities
    -- 5) result should have this finalised cell added
    where (groupsWithThisCell, otherGroups) = partition (Set.member cellID) (groups board)
          filteredGroups = map (Set.filter (/= cellID)) groupsWithThisCell
          touchedCellIDs = smoosh filteredGroups

-- union of a whole bunch of sets
smoosh :: Ord a => [Set a] -> Set a
smoosh = foldr Set.union Set.empty

-- make the cell and all associated cells dirty
dishDirt :: CellID -> Board -> Board
dishDirt cellID board = board { dirty = newDirty }
    where newDirty = foldr Set.union (dirty board) dirtyGroups
          dirtyGroups = filter (Set.member cellID) (groups board)

groupsContaining :: CellID -> [Group] -> [Group]
groupsContaining cellID = filter (Set.member cellID)

sampleBoard = [
    ("11", '5'),
    ("12", '8'),
    ("22", '3'),
    ("23", '1'),
    ("24", '4'),
    ("28", '7'),
    ("32", '2'),
    ("35", '6'),
    ("36", '7'),
    ("42", '1'),
    ("44", '6'),
    ("45", '2'),
    ("47", '7'),
    ("48", '8'),
    ("52", '5'),
    ("58", '2'),
    ("62", '4'),
    ("63", '2'),
    ("65", '1'),
    ("66", '8'),
    ("74", '1'),
    ("75", '4'),
    ("78", '5'),
    ("82", '6'),
    ("86", '5'),
    ("87", '2'),
    ("88", '9'),
    ("98", '1'),
    ("99", '3')
    ]

main = do
    print $ createPossibilities
    print $ createRows
    print $ createCols
    print $ createSquares
    print $ createBoard
    let board2 = foldr (\(cellID, value) board -> setCell cellID value board) createBoard sampleBoard
    print $ board2

{-
reduce :: Board -> Maybe Board
reduce board =
    let dirt = dirty board in
        if Set.null dirt
            then Nothing
            else if Set.size first <= 1
                then reduceCell (findMin first) board
                else board { dirty = (snd . deleteFindMin) (dirty board) }
                -}
