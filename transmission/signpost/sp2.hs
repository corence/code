
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

puzzleUnwrapDirs :: [(CellID, Int, String)] -> [(CellID, Int, [CellID])]
puzzleUnwrapDirs protoCells = map (\(cellID, value, direction) -> (cellID, value, expand protoCells direction cellID)) protoCells
          where expand :: [(CellID, Int, String)] -> String -> CellID -> [CellID]
                expand protoCells direction sourceCellID
                  | sourceCellID == newCellID = []
                  | (length $ filter (\(oldCellID, oldValue, _) -> oldCellID == newCellID && oldValue == 1) protoCells) > 0 = expand protoCells direction newCellID
                  | null $ filter (\(oldCellID, _, _) -> oldCellID == newCellID) protoCells = []
                  | otherwise = newCellID : (trace ("cool, new cell " ++ newCellID) $ expand protoCells direction newCellID)
                    where newCellID = move direction sourceCellID
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
