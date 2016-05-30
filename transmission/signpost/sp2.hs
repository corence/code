
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Solver
import Signpost
import SignpostPuzzles
import ListUtil

initSolver :: Board -> Solver
initSolver board = Solver {
    openPlans = [initialPlan],
    terminalSteps = [],
    guesses = []
}
    where initialPlan = Plan {
        planAction = Action { actionName = "init", actionTransformer = (\_ -> board), actionBoard = [] },
        planParent = InitStep { stepId = "init", stepPostBoard = [] }
    }

main = do
    let solver = initSolver $ (\b -> (trace $ formatList b) b) $ puzzleToBoard $ puzzleUnwrapDirs puzzle7
    putStrLn $ "initial state: "
    putStrLn $ formatList $ stepPostBoard $ planParent $ head $ openPlans $ solver
    putStrLn $ "initial plan: "
    putStrLn $ show $ openPlans $ solver
    putStrLn $ "initial board: "
    putStrLn $ show $ stepPostBoard $ planParent $ head $ openPlans $ solver
    putStrLn $ "first board: "
    putStrLn $ formatList $ (actionTransformer $ planAction $ head $ openPlans $ solver) []
    let solution = solve solver
    putStrLn $ "done solved that"
    putStrLn $ "num things left to solve: " ++ (show $ length $ openPlans solution)
    putStrLn $ "first terminal: " ++ (show $ head $ terminalSteps solution)
    putStrLn $ "guesses: " ++ (show $ guesses solution)
    putStrLn $ "terminals: " ++ (show $ terminalSteps solution)
    putStrLn $ "--------"
    putStrLn $ "--------"
    putStrLn $ "--------"
    putStrLn $ "--------"
    putStrLn $ "boards: " ++ (concat $ map formatList $ map stepPostBoard (terminalSteps solution))
