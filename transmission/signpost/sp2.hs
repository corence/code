
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Solver
import Signpost
import SignpostPuzzles

puzzleToBoard :: [(CellID, Int, [CellID])] -> Board
puzzleToBoard protoCells = map reify protoCells
    where reify (cellID, value, outputs) = Chain {
        cid = cellID, chainCells = [cellID],
        chainValue = value, chainLength = 1,
        chainOutputs = outputs, chainInputs = (inputs cellID)
    }
          inputs cellID = map (\(cellID, _, _) -> cellID) (filter (\(_, _, outputs) -> elem cellID outputs) protoCells)

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
    let solver = (initSolver (puzzleToBoard puzzle5))
    let solution = solve solver
    putStrLn $ "done solved that"
    putStrLn $ "num things left to solve: " ++ (show $ length $ openPlans solution)
    putStrLn $ "first terminal: " ++ (show $ head $ terminalSteps solution)
