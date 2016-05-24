
module Solver
( Solver(Solver, openPlans, terminalSteps, guesses)
, Plan(Plan, planAction, planParent)
, Step(Step, InitStep, stepActions, stepPreBoard, stepPostBoard, stepParent, stepId)
, solve
, resolveAction
) where

import Signpost
import ListUtil

data Plan = Plan {
    planAction :: Action,
    planParent :: Step
}

instance Show Plan where
    show plan = "{" ++ (stepId (planParent plan)) ++ " :: " ++ show (planAction plan) ++ "}"

data Step = InitStep {
    stepId :: String,
    stepPostBoard :: Board
} | Step {
    stepId :: String,
    stepActions :: [Action],
    stepPreBoard :: Board,
    stepPostBoard :: Board,
    stepParent :: Step
}

instance Show Step where
    show step = "{" ++ stepId step ++ "}"

data Solver = Solver {
    openPlans :: [Plan],
    terminalSteps :: [Step],
    guesses :: [Action]
}

solve :: Solver -> Solver
solve solver
  | length (openPlans solver) <= 0 = solver
  | otherwise = solve $ Solver {
      openPlans = newPlans ++ plans,
      terminalSteps = terminals,
      guesses = (guesses solver) ++ newGuesses
      }
    where newGuesses = guess (stepPostBoard newStep)
          newPlans = map (makePlan newStep) newGuesses
          (plan, plans) = selectPlan (openPlans solver)
          newStep = resolvePlan $ trace ("resolving plan " ++ show plan) plan
          terminals
            | null newPlans = newStep : terminalSteps solver
            | otherwise = terminalSteps solver

resolvePlan :: Plan -> Step
resolvePlan plan = Step {
                        stepActions = actions,
                        stepPreBoard = preBoard,
                        stepPostBoard = postBoard,
                        stepParent = parent,
                        stepId = (stepId parent) ++ " -> " ++ (actionName $ planAction plan)
                    }
                    where (actions, postBoard) = resolveAction (planAction plan) preBoard
                          parent = planParent plan
                          preBoard = stepPostBoard parent

selectPlan :: [Plan] -> (Plan, [Plan])
selectPlan (plan:plans) = (plan, plans)

resolveAction :: Action -> Board -> ([Action], Board)
resolveAction action board = (action : followups, finalBoard)
    where newBoard = (actionTransformer action) (verifyBoard action board)
          (followups, finalBoard) = case (react newBoard) of
            Just reaction -> resolveAction reaction newBoard
            Nothing -> ([], newBoard)

guess :: Board -> [Action]
guess board = (\guesses -> trace ("generated guesses: " ++ show guesses) guesses) $ concat (map linkToEveryOutput board)
    where linkToEveryOutput chain = map (makeAction (cid chain)) (chainOutputs chain)
          makeAction :: CellID -> CellID -> Action
          makeAction source target = Action { actionName = source ++ "->" ++ target, actionTransformer = linkChainsInBoard source target, actionBoard = board }

makePlan :: Step -> Action -> Plan
makePlan parent action = Plan {
    planAction = action,
    planParent = parent
    }
    
react :: Board -> Maybe Action
react board
  | length results > 0 = trace ("----------------------------------------------\nreacted: " ++ (show (head results)) ++ "\n----------------------------------------------") $ Just (head results)
  | otherwise = Nothing
  --where results = catMaybes (map (reactSingleOutput board) board)
  where results :: [Action]
        results = (convergeMaybes [
            map (reactSingleOutput board) board,
            map (reactSingleInput board) board
            ]) ++ (concat $ map (reactConsecutiveValues board) board)

reactSingleOutput :: Board -> Chain -> Maybe Action
reactSingleOutput board chain =
    if ((length (chainOutputs chain)) == 1)
        then Just $ Action { actionName = "single output " ++ cid chain ++ "=>" ++ (head (chainOutputs chain)), actionTransformer = (linkChainsInBoard (cid chain) (head (chainOutputs chain))), actionBoard = board }
        else Nothing

reactSingleInput :: Board -> Chain -> Maybe Action
reactSingleInput board chain =
    if ((length (chainInputs chain)) == 1)
        then Just $ Action { actionName = "single input " ++ (head (chainInputs chain)) ++ "=>" ++ (cid chain), actionTransformer = (linkChainsInBoard (head (chainInputs chain)) (cid chain)), actionBoard = board }
        else Nothing

reactConsecutiveValues = reactConsecutiveValuesSupreme

-- for the given chain
  -- for each of its outputs as a chain
    -- if this and that chain match
      -- then add a new action to the results
reactConsecutiveValuesCombinators :: Board -> Chain -> [Action]
reactConsecutiveValuesCombinators board chain = linksFromPairs matchingPairs
    where linksFromPairs = map (\(sourceID, targetID) -> makeLinkAction sourceID targetID)
          matchingPairs = map (\outputID -> ((cid chain), outputID)) matchingOutputs
          matchingOutputs = filter (\outputID -> isConsecutive chain (getChain outputID board)) (chainOutputs chain)
          makeLinkAction sourceID targetID = Action { actionName = "consecutive values " ++ sourceID ++ "=>" ++ targetID, actionTransformer = (linkChainsInBoard sourceID targetID), actionBoard = board }
          isConsecutive source target = (chainValue source + chainLength source) == chainValue target
          
reactConsecutiveValuesFold :: Board -> Chain -> [Action]
reactConsecutiveValuesFold board chain = foldr makeMatchingLinks [] (chainOutputs chain)
    where makeMatchingLinks outputID results = if isConsecutive chain output
                then makeLinkAction (cid chain) outputID : results
                else results
                    where output = getChain outputID board
                          makeLinkAction sourceID targetID = Action { actionName = "consecutive values " ++ sourceID ++ "=>" ++ targetID, actionTransformer = (linkChainsInBoard sourceID targetID), actionBoard = board }
                          isConsecutive source target = (chainValue source + chainLength source) == chainValue target
                          
reactConsecutiveValuesSupreme :: Board -> Chain -> [Action]
reactConsecutiveValuesSupreme board chain = map (makeLinkAction (cid chain)) $ filter (\outputID -> isConsecutive chain (getChain outputID board)) $ chainOutputs chain
    where makeLinkAction :: CellID -> CellID -> Action
          makeLinkAction sourceID targetID = Action { actionName = "consecutive values " ++ sourceID ++ "=>" ++ targetID, actionTransformer = (linkChainsInBoard sourceID targetID), actionBoard = board }
          isConsecutive :: Chain -> Chain -> Bool
          isConsecutive source target = (chainValue source + chainLength source) == chainValue target
