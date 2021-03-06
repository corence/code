
module Puzzle
( Puzzle (..)
, PuzzleCell
, ProtoCell
, puzzleToBoard
, puzzleUnwrapDirs
) where

import Signpost

type PuzzleCell = (CellID, Int, [CellID])
data Puzzle = Puzzle [PuzzleCell]

puzzleToBoard :: Puzzle -> Board
puzzleToBoard puzzle = map reify puzzleCells
    where reify (cellID, value, outputs) = Chain {
        cid = cellID, chainCells = [cellID],
        chainValue = value, chainLength = 1,
        chainOutputs = outputs, chainInputs = (inputs cellID)
    }
          inputs cellID = map (\(cellID, _, _) -> cellID) (filter (\(_, _, outputs) -> elem cellID outputs) puzzleCells)
          Puzzle puzzleCells = puzzle

type ProtoCell = (CellID, Int, String)

puzzleUnwrapDirs :: [ProtoCell] -> [PuzzleCell]
puzzleUnwrapDirs protoCells = map (\(cellID, value, direction) -> (cellID, value, expand protoCells (cellID, value, direction) cellID)) protoCells
          -- expand: get all the CellID along the given direction from sourceCell
          where expand :: [ProtoCell] -> ProtoCell -> CellID -> [CellID]
                expand protoCells (sourceCellID, sourceCellValue, direction) prevCellID
                  -- we didn't move -- we are in a loop (probably because this is the final cell) so just return nothing
                  | prevCellID == newCellID = []
                  -- the newly generated cell doesn't exist (probably is outside the grid) -- return nothing
                  | null $ filter (\(oldCellID, _, _) -> oldCellID == newCellID) protoCells = []
                  -- the new cell value doesn't follow from the source cell -- skip over it
                  | (newCellValue > 0) && (sourceCellValue > 0) && (newCellValue /= sourceCellValue + 1) = expand protoCells (sourceCellID, sourceCellValue, direction) newCellID
                  -- the new cell has a value of 1
                  | (newCellValue == 1) = expand protoCells (sourceCellID, sourceCellValue, direction) newCellID
                  -- the new cell is fine -- include it in the results
                  | otherwise = newCellID : expand protoCells (sourceCellID, sourceCellValue, direction) newCellID
                    where newCellID = move direction prevCellID
                          newCellValue = head $ getProtoCellValues newCellID protoCells
                          getProtoCellValues :: CellID -> [(CellID, Int, String)] -> [Int]
                          getProtoCellValues cellID protoCells = map (\(_, value, _) -> value) (filter (\(oldCellID, _, _) -> oldCellID == cellID) protoCells)
                move :: String -> CellID -> CellID
                move direction [x, y] = [newX, newY]
                    where newX
                            | elem 'n' direction = pred x
                            | elem 's' direction = succ x
                            | otherwise = x
                          newY
                            | elem 'w' direction = pred y
                            | elem 'e' direction = succ y
                            | otherwise = y
          
          {-
          move direction [x, y] = [
              where nx = case direction of
                            "nw", "w", "sw" -> pred x
                            "n", "", "s" -> x
                            "ne", "e", "se" -> succ x
                            otherwise -> error "bad move: " ++ direction ++ " from " ++ [x, y]
                    ny = case direction of
                            "" -> x]
                            -}
                

