
module Solver
( 
) where

import Signpost
import Debug.Trace
import ListUtil

data Action = Action {
    actionName :: String,
    actionTransformer :: Board -> Board,
    actionBoard :: Board
}

instance Show Action where
    show action = actionName action

data Plan = Plan {
    planAction :: Action,
    planParent :: Step
}

data Step = InitStep Board | Step {
    stepActions :: [Action],
    stepPreBoard :: Board,
    stepPostBoard :: Board,
    stepParent :: Step,
    stepId :: String
}

data Solver = Solver {
    openPlans :: [Plan],
    terminalSteps :: [Step]
}

solve :: Solver -> Solver
solve solver
  | length (openPlans solver) <= 0 = solver
  | otherwise = solve $ Solver {
      openPlans = newPlans ++ plans,
      terminalSteps = terminals
      }
    where newPlans = map (makePlan newStep) (guess (stepPostBoard newStep))
          (plan, plans) = selectPlan (openPlans solver)
          newStep = resolvePlan plan
          terminals
            | null newPlans = newStep : terminalSteps solver
            | otherwise = terminalSteps solver

resolvePlan :: Plan -> Step
resolvePlan plan = error "not implemented"

selectPlan :: [Plan] -> (Plan, [Plan])
selectPlan (plan:plans) = (plan, plans)

resolveAction :: Action -> Board -> ([Action], Board)
resolveAction action board = error "not done"

guess :: Board -> [Action]
guess board = concat (map linkToEveryOutput board)
    where linkToEveryOutput chain = map (makeAction (cid chain)) (chainOutputs chain)
          makeAction :: CellID -> CellID -> Action
          makeAction source target = Action { actionName = "wild guess", actionTransformer = replaceLinkChains source target, actionBoard = board }

makePlan :: Step -> Action -> Plan
makePlan parent action = Plan {
    planAction = action,
    planParent = parent
    }
    
replaceLinkChains :: CellID -> CellID -> Board -> Board
replaceLinkChains chain1ID chain2ID =
    makeAction
        where makeAction board = trace ("making action " ++ (show $ newChain : remainder)) $ newChain : remainder
                where newChain = verifyChain $ linkChains chain1 chain2
                      remainder1 = trace ("replacing chain references from " ++ chain1ID ++ " to " ++ chain2ID ++ " against " ++ (formatList board)) $ map (replaceChainReferences chain1ID chain2ID) (filter (\c -> (cid c) /= chain1ID && (cid c) /= chain2ID) board)
                      remainder = trace ("gonna disconnectValueMismatches on " ++ (cid newChain) ++ " against " ++ (formatList remainder1)) $ disconnectValueMismatches (cid newChain) remainder1
                      remainder :: Board
                      chain1 = trace ("getting chain 1 from board " ++ (formatList board)) $ getChain chain1ID board
                      chain2 = getChain chain2ID board

verifyChain :: Chain -> Chain
verifyChain chain
  | (chainValue chain == 1) && (not (null (chainInputs chain))) = error $ "bad chain -- first chain shouldn't have inputs:\n" ++ (show chain)
  | (chainValue chain + chainLength chain > 25) && (not (null (chainOutputs chain))) = error $ "bad chain -- last chain shouldn't have outputs:\n" ++ (show chain)
  | otherwise = trace ("nascent chain: " ++ (show chain)) chain

disconnectValueMismatches :: CellID -> Board -> Board
disconnectValueMismatches chainID board =
    let boardA = foldr (\outputID board1 -> disconnectIfValueMismatch chain (getChain outputID board1) board1) board (chainOutputs chain) in
        foldr (\inputID board1 -> disconnectIfValueMismatch (getChain inputID board1) chain board1) boardA (chainInputs chain)
    where disconnectIfValueMismatch :: Chain -> Chain -> Board -> Board
          disconnectIfValueMismatch chain1 chain2 board =
              if valueMismatch chain1 chain2
                  then trace ("dropping outputs") $ dropOutputs (cid $ trace ("cidding chain1") chain1) (cid chain2) (dropInputs (cid chain2) (cid chain1) board)
                  else board
          dropOutputs :: CellID -> CellID -> Board -> Board
          dropOutputs chainID outputID board1 = traceShowId $ map (\c -> replaceChain chainID chain { chainOutputs = filter (/= outputID) (chainOutputs chain) } c) board1
          dropInputs chainID inputID board1 = traceShowId $ map (\c -> replaceChain chainID chain { chainInputs = filter (/= inputID) (chainInputs chain) } c) board1
          chain = trace ("getting da chain " ++ chainID ++ " from board " ++ (formatList board)) $ getChain chainID board

valueMismatch :: Chain -> Chain -> Bool
valueMismatch chain1 chain2
  | (chainValue chain1 == 0) || (chainValue chain2 == 0) = False
  | (chainValue chain1 + chainLength chain1) /= (chainValue chain2) = False
  | otherwise = True

