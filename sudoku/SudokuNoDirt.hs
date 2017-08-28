
module SudokuNoDirt where

import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Control.Applicative

import Debug.Trace

import Data.List
import Data.Maybe
import Data.Function

(<&>) = flip (<$>)

type CellID = String
type Group = Set CellID
type Possibilities = Set Char

data Solver = Solver {
    possibilities :: Map CellID Possibilities,
    groups :: [Group],
    output :: Map CellID Char
    } deriving (Show)

cutPossibilityFromCell :: Char -> CellID -> Map CellID Possibilities -> Map CellID Possibilities
cutPossibilityFromCell value cellID = Map.update (Just . Set.delete value) cellID

-- union of a whole bunch of sets
concatSets :: Ord a => [Set a] -> Set a
concatSets = foldr Set.union Set.empty

-- 0) if the new value is a char that isn't in the possibilities, abort
-- 1) remove it from possibilities
-- 2) find which groups it belongs to
-- 3) every cell in every group should have cutPossibility called on it, and if that succeeds then it should also become dirty. those groups should also have the cell id purged
-- 3a) for each group that it fell into
-- 3b) cleanse it from that group
-- 3c) for each cell in those groups
-- 3c1) cutPossibility of this cell in those groups
-- 3c2) if it existed, dirty this cell
setValue :: Char -> CellID -> Solver -> Solver
setValue value cellID solver
    = (addOutput cellID value . removeCellFromGroups cellID . removeValueFromGroupPossibilities (Set.singleton cellID) value . removeCellIDFromPossibilities cellID) solver

addOutput :: CellID -> Char -> Solver -> Solver
addOutput cellID value solver = solver { output = Map.insert cellID value (output solver) }

removeCellIDFromPossibilities :: CellID -> Solver -> Solver
removeCellIDFromPossibilities cellID solver
    = solver { possibilities = Map.delete cellID (possibilities solver) }

removeCellFromGroups :: CellID -> Solver -> Solver
removeCellFromGroups cellID solver
    = solver { groups = map (Set.delete cellID) (groups solver) }

removeValueFromGroupPossibilities :: Set CellID -> Char -> Solver -> Solver
removeValueFromGroupPossibilities cellIDs value solver
    = solver { possibilities = Set.foldr (cutPossibilityFromCell value) (possibilities solver) relevantCells }
        where relevantGroups = filter (not . Set.null . Set.intersection cellIDs) (groups solver)
              relevantCells = concatSets relevantGroups
{-
    solver {
        possibilities = Map.delete cellID newPossibilities,
        groups = map (Set.delete cellID) (groups solver),
        output = Map.insert cellID value (solver output)
        }
    where newPossibilities = cutPossibility value (Map.delete cellID (possibilities solver))
    -}

dropValueFromPossibilities :: Char -> Group -> Map CellID Possibilities -> Map CellID Possibilities
dropValueFromPossibilities value group possibilities = foldr (Map.update (Just . Set.delete value)) possibilities group


-- Initialisation --
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

allCellIDs :: [CellID]
allCellIDs = liftA2 (\x y -> x : y : []) ['1'..'9'] ['1'..'9']

createPossibilities :: Map CellID Possibilities
createPossibilities = Map.fromList possibilities
    where possibilities = map (\cellID -> (cellID, Set.fromList ['1'..'9'])) allCellIDs

createSolver :: Solver
createSolver = Solver {
    possibilities = createPossibilities,
    groups = createRows ++ createCols ++ createSquares,
    output = Map.empty
    }

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

-- the data to be represented is a row of cells. The first few are
-- [could be anything, has to be 5, could be any of "3468", 2 or 4]
-- before this function starts it's converted to a list of strings, max length 3 in each one.
-- [["123","456","789"],["   "," 5 "],["346","8"],["24"]]
-- so this is a list of gridcells. max length 9.
-- first we transpose so that each cell is split across 3 output rows
-- [["123","   ","346","24"],["456"," 5 ","8"],["789"]]
-- then we intercalate

-- output format is like:
-- "123     346\n456  5  8\n346 8"
displayRow :: [[String]] -> String
displayRow = (unlines . map (intercalate "|") . transpose)

display :: Solver -> String
display solver = (intercalate (replicate (9*4-1) '-' ++ "\n") . map displayRow . groupRows . map cellStringToGrid . map (pad ' ' 9) . map (cellToString solver)) allCellIDs

cellToString :: Solver -> CellID -> String
cellToString solver cellID = (head . catMaybes) [displayOutputCell cellID, displayCellPossibilities cellID, Just "         "]
    where displayOutputCell cellID = Map.lookup cellID (output solver) <&> (\value -> "    " ++ [value] ++ "    ")
          displayCellPossibilities cellID = Map.lookup cellID (possibilities solver) <&> Set.toList

groupRows :: [[String]] -> [[[String]]]
groupRows = split 9

cellStringToGrid :: String -> [String]
cellStringToGrid = split 3

pad :: a -> Int -> [a] -> [a]
pad filler len list = list ++ replicate (len - length list) filler

split :: Int -> [a] -> [[a]]
split _ [] = []
split rowLength list = take rowLength list : split rowLength (drop rowLength list)

reduce :: Solver -> Solver
reduce = reduceUniques . reduceClusters

-- if a value only appears in one place in a group, solidify it
-- a "unique" possibility is a possibility that only appears in one place in a group
reduceUniques :: Solver -> Solver
reduceUniques solver = foldr reduceUniquesInGroup solver (groups solver)

-- given a group and a solver,
reduceUniquesInGroup :: Group -> Solver -> Solver
reduceUniquesInGroup group solver
    = Map.foldrWithKey setValue solver uniquesToCellIDs
        where uniquesToCellIDs = findUniquePossibilitiesInGroup group (possibilities solver)

-- for all the cells in the group, find the cells that have a unique possibility
findUniquePossibilitiesInGroup :: Group -> Map CellID Possibilities -> Map Char CellID
findUniquePossibilitiesInGroup group possibilities
    = (Map.map Set.findMin . Map.filter ((== 1) . Set.size)) (groupPossibleValuesToCellIDs possibilities group)

-- 1) for each member of the group:
-- 1a) get all the possible values of it
-- 1b) for each of those possible values:
-- 1b1) add it to the Map
--
-- in pipeline form that's:
-- 1) group :: Set CellID
-- 1a) map :: CellID -> (CellID, Set Char)
-- 2) groupWithValues :: Set (CellID, Set Char)
-- 2a) func :: (CellID, Set Char) -> Map Char (Set CellID) -> Map Char (Set CellID)
-- 3) Map Char (Set CellID)
groupPossibleValuesToCellIDs :: Map CellID Possibilities -> Set CellID -> Map Char (Set CellID)
groupPossibleValuesToCellIDs possibilities group
    = (foldr addToMap Map.empty . Set.foldr expandCellValuePairs [] . Set.map attachValues) group
        where attachValues :: CellID -> (CellID, Set Char)
              attachValues cellID = maybe (cellID, Set.empty) ((,)cellID) (Map.lookup cellID possibilities)
              expandCellValuePairs :: (CellID, Set Char) -> [(CellID, Char)] -> [(CellID, Char)]
              expandCellValuePairs (cellID, values) result = Set.foldr (\value -> ((cellID, value) :)) result values
              addToMap :: (CellID, Char) -> Map Char (Set CellID) -> Map Char (Set CellID)
              addToMap (cellID, value) = Map.insertWith Set.union value (Set.singleton cellID)

-- if a tuple of cells has that many possibilities, clean out the other cells within that group
-- 1) foreach group
-- 2) get all values for each cell
-- 3) foreach cell
-- 4) how many cells are a subset of this one? if n or less, then act
--
-- act:
-- 1) given a tuple cluster
-- 2) every cell that is not in this cluster
-- 3) delete the cluster elements from it
type Cluster = (Set CellID, Set Char)
reduceClusters :: Solver -> Solver
reduceClusters = id

main = do
    print createSolver
    let solver2 = foldr (\(cellID, value) solver -> setValue value cellID solver) createSolver sampleBoard
    print solver2
    putStrLn " "
    putStrLn $ display solver2
    putStrLn $ display $ reduce solver2
