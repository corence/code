
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Solver
import Signpost
import SignpostPuzzles
import ListUtil

puzzleToBoard :: [(CellID, Int, [CellID])] -> Board
puzzleToBoard protoCells = map reify protoCells
    where reify (cellID, value, outputs) = Chain {
        cid = cellID, chainCells = [cellID],
        chainValue = value, chainLength = 1,
        chainOutputs = outputs, chainInputs = (inputs cellID)
    }
          inputs cellID = map (\(cellID, _, _) -> cellID) (filter (\(_, _, outputs) -> elem cellID outputs) protoCells)

type ProtoCell = (CellID, Int, String)
type PuzzleCell = (CellID, Int, [CellID])

puzzleUnwrapDirs :: [ProtoCell] -> [PuzzleCell]
puzzleUnwrapDirs protoCells = map (\(cellID, value, direction) -> (cellID, value, expand protoCells (cellID, value, direction) cellID)) protoCells
          -- expand: get all the CellID along the given direction from sourceCell
          where expand :: [ProtoCell] -> ProtoCell -> CellID -> [CellID]
                expand protoCells (sourceCellID, sourceCellValue, direction) prevCellID
                  -- we didn't move -- we are in a loop (probably because this is the final cell) so just return nothing
                  | prevCellID == newCellID = []
                  -- the newly generated cell doesn't exist (probably is outside the grid) -- return nothing
                  | null $ filter (\(oldCellID, _, _) -> oldCellID == newCellID) protoCells = []
                  -- the new cell doesn't follow from the source cell -- skip over it
                  | newCellValue > 0 && (newCellValue /= sourceCellValue + 1) = expand protoCells (sourceCellID, sourceCellValue, direction) newCellID
                  -- the new cell is fine -- include it in the results
                  | otherwise = newCellID : (trace ("cool, new cell " ++ newCellID) $ expand protoCells (sourceCellID, sourceCellValue, direction) newCellID)
                    where newCellID = move direction prevCellID
                          newCellValue = head $ getProtoCellValues newCellID protoCells
                          getProtoCellValues :: CellID -> [(CellID, Int, String)] -> [Int]
                          getProtoCellValues cellID protoCells = map (\(_, value, _) -> value) (filter (\(oldCellID, _, _) -> oldCellID == cellID) protoCells)
                move :: String -> CellID -> CellID
                move direction [x, y] = trace ((show [newX, newY]) ++ " is new for direction " ++ direction) $ [newX, newY]
                    where newX
                            | elem 'w' direction = pred x
                            | elem 'e' direction = succ x
                            | otherwise = x
                          newY
                            | elem 'n' direction = pred y
                            | elem 's' direction = succ y
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
                

initSolver :: Board -> Solver
initSolver board = Solver {
    openPlans = [initialPlan],
    terminalSteps = []
}
    where initialPlan = Plan {
        planAction = Action { actionName = "init", actionTransformer = (\_ -> board), actionBoard = [] },
        planParent = InitStep { stepId = "init", stepPostBoard = [] }
    }

main = do
    let solver = initSolver $ (\b -> (trace $ formatList b) b) $ puzzleToBoard $ puzzleUnwrapDirs puzzle7
    let solution = solve solver
    putStrLn $ "done solved that"
    putStrLn $ "num things left to solve: " ++ (show $ length $ openPlans solution)
    putStrLn $ "first terminal: " ++ (show $ head $ terminalSteps solution)
