
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

type CellID = String
type Cell = (CellID, Int, [CellID], [CellID]) -- cell ID, value, outputs, inputs

data Chain = Chain {
    cid :: CellID,
    chainCells :: [CellID],
    chainValue :: Int,
    chainLength :: Int,
    chainOutputs :: [CellID],
    chainInputs :: [CellID]
} deriving (Show)

type Board = [Chain]

formatList :: Show a => [a] -> String
formatList [] = "\n"
formatList (x:xs) = "\n" ++ (show x) ++ formatList xs

--type Action = (CellID, (Chain -> Chain))
data Action = Action {
    actionName :: String,
    actionAction :: Board -> Board,
    actionBoard :: Board
}

instance Show Action where
    show action = actionName action

type ManeuverID = String
data Maneuver = Maneuver {
        mid :: ManeuverID,
        maneuverParent :: ManeuverID,
        maneuverAction :: Action,
        maneuverReactions :: [Action]
    }

instance Show Maneuver where
    show maneuver = "{m#" ++ mid maneuver ++ "reactions:" ++ (show (maneuverReactions maneuver)) ++ "}"

data SolveState = SolveState {
    stateHistory :: [Maneuver],
    stateManeuvers :: Map ManeuverID Maneuver,
    statePossibleActions :: [Action]
    }

instance Show SolveState where
    show state = "state {maneuvers:" ++ show (stateManeuvers state) ++ ", actions:" ++ show (statePossibleActions state) ++ "}"

-- solving:
-- generate a tree
--   Maneuver { id: 1, action: init board, board: (the board), reactions: [everything that happens auto] }
--   Maneuver { id: 1-1, parent: 1, action: (first thing you could do after 1), board: (the board after reactions), reactions: [everything that happens auto] }
--   Maneuver { id: 1-2, parent: 1, action: (second thing you could do after 1), board: (board), reactions: [re] }

solve :: SolveState -> SolveState
solve state = maybe state solve (solveStep state)

solvePrintingly :: SolveState -> SolveState
solvePrintingly state =
    --trace ("solvePrintingly " ++ (show state)) (maybe state id (solveStep state))
    trace ("solvePrintingly " ++ (show state)) (maybe state solvePrintingly (solveStep state))
    
--solvePrintingly state = maybe (return state) (\state -> putStrLn ("printingly" ++ (show state))

-- given a Maneuver:
--   - select & remove one of the nextActions. (could be breadth-first, depth-first, or maybe a-star)
--   - for the nextAction:
--     - generate every reaction exhaustively
--     - save the updated board
--     - put this complete Maneuver in the "maneuvers" map
--     - generate all possible follow-up actions. Save these as incomplete maneuvers in "nextActions". (incomplete = id + action + parent, nothing more)
-- so:
--   - maneuvers :: Map (maneuverID, maneuver)
--   - nextActions :: [maneuver]
-- and after each run of this function, the following changes happen:
--   - one thing removed from nextActions
--   - it is completed and put in maneuvers
--   - all its followup actions go in nextActions
solveStepRetarded :: SolveState -> Maybe SolveState
solveStepRetarded state
  | length (statePossibleActions state) <= 0 = trace ("skipped a step") Nothing
  | otherwise =
    let (maneuver:newPossibles) = statePossibleActions state in
        -- assume maneuver is incomplete, and looks like this:
        -- data Maneuver = Maneuver {
        --      mid :: ManeuverID,
        --      maneuverParent :: ManeuverID,
        --      maneuverAction :: Action,
        --      maneuverReactions :: [Action]
        --    }

        let maneuver1 = maneuver { maneuverBoardAfter = (actionAction (maneuverAction maneuver)) (maneuverBoardBefore maneuver) } :: Maneuver in
            let maneuver2 = trace ("maneuver is up, new board is: " ++ (formatList (maneuverBoardAfter maneuver1)) ++ ", generating reactions") $ generateReactions maneuver1 :: Maneuver in
                let followups = trace ("applying reactions against maneuverBoardAfter maneuver2 -- " ++ (formatList (maneuverBoardAfter maneuver2))) $ act (maneuverBoardAfter maneuver2) :: [Action] in
                    let followupManeuvers = trace ("generating followups from " ++ (show followups)) $ actionsToManeuvers (maneuverBoardAfter maneuver2) (maneuverParent maneuver2) followups in
                        trace ("return actions, followup: " ++ show followupManeuvers ++ ", newP: " ++ show newPossibles) Just SolveState {
                            stateManeuvers = Map.insert (mid maneuver2) maneuver2 (stateManeuvers state),
                            statePossibleActions = followupManeuvers ++ newPossibles
                        }
        
        
solveStep :: SolveState -> Maybe SolveState
solveStep state
  | length (statePossibleActions state) <= 0 = trace ("skipped a step") Nothing
  | otherwise = Just SolveState {
        stateHistory = newManeuver : (stateHistory state),
        stateManeuvers = Map.insert (mid newManeuver) newManeuver (stateManeuvers state),
        statePossibleActions = act finalBoard ++ statePossibleActions state
      }
    where (action:actions) = statePossibleActions state -- choose an action
          finalBoard = (actionAction action) (actionBoard action)
          
          

-- don't store Cells, store Chains. This is just like a Cell but it has a list of IDs (and also a ChainID which is the CellID of the first cell)

-- actions look like this: [ ["a1", (Cell -> Maneuver)], ["a2", (Maneuver -> Maneuver) ] ]
-- so do reactions

-- when we select nextAction we do:
generateReactions :: Maneuver -> Maneuver
generateReactions maneuver =
    -- x) create boardAfterAction (no)
    -- 2) create reactions and reduce across them (foldr)
    -- 3) save this final board
    -- output: all the reactions and the final board (wrapped in a new maneuver)
    
    -- while(true)
    --   make reaction
    --   make board

    let board = (maneuverBoardAfter maneuver) in
        maybe maneuver (\reaction -> generateReactions (maneuver { maneuverBoardAfter = (actionAction reaction) board, maneuverReactions = reaction : (maneuverReactions maneuver) } )) (react board)

act :: Board -> [Action]
act board = concat (map linkToEveryOutput board)
    where linkToEveryOutput chain = map (\output -> Action { actionName = "wild guess", actionAction = replaceLinkChains (cid chain) output }) (chainOutputs chain)

actionsToManeuvers :: Board -> ManeuverID -> [Action] -> [Maneuver]
actionsToManeuvers board parentID actions =
    zipWith (\action index -> actionToManeuver board parentID index action) actions (iterate (+ 1) 1)

actionToManeuver :: Board -> ManeuverID -> Int -> Action -> Maneuver
actionToManeuver board parentID index action = Maneuver {
        mid = parentID ++ "-" ++ (show index),
        maneuverParent = parentID,
        maneuverAction = action,
        maneuverReactions = [],
        maneuverBoardBefore = board,
        maneuverBoardAfter = board
    }

react :: Board -> Maybe Action
react board
  | length results > 0 = Just (head results)
  | otherwise = Nothing
  --where results = catMaybes (map (reactSingleOutput board) board)
  where results = convergeMaybes [
         map (reactSingleOutput board) board,
         map (reactSingleInput board) board
         ]

convergeMaybes :: [[Maybe a]] -> [a]
convergeMaybes maybeList = foldr (\maybes justs -> prependMaybes justs maybes) [] maybeList

prependMaybes :: [a] -> [Maybe a] -> [a]
prependMaybes justs maybes = foldr addIfHasValue justs maybes
    where addIfHasValue :: Maybe b -> [b] -> [b]
          addIfHasValue possibleValue justies = maybe justies (prepend justies) possibleValue

prepend :: [a] -> a -> [a]
prepend list element = element : list

reactSingleOutput :: Board -> Chain -> Maybe Action
reactSingleOutput board chain =
    if ((length (chainOutputs chain)) == 1)
        then Just $ Action { actionName = "single output", actionAction = (replaceLinkChains (cid chain) (head (chainOutputs chain))) }
        else Nothing

reactSingleInput :: Board -> Chain -> Maybe Action
reactSingleInput board chain =
    if ((length (chainInputs chain)) == 1)
        then Just $ Action { actionName = "single input", actionAction = (replaceLinkChains (head (chainInputs chain)) (cid chain)) }
        else Nothing

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

--convergeMaybes maybeList = foldr (\maybes justs -> prependMaybes justs maybes) [] maybeList
  -- for each input/output,
  --   if the two chains have a value mismatch,
  --     then drop them from each other's input/output
  

-- replace references from the old chain to the new chain
replaceChainReferences :: CellID -> CellID -> Chain -> Chain
replaceChainReferences chain1ID chain2ID chain = chain {
    chainInputs = map (replaceThing chain2ID chain1ID) (chainInputs chain),
    chainOutputs = filter (/= chain2ID) (chainOutputs chain)
}

verifyChain :: Chain -> Chain
verifyChain chain
  | (chainValue chain == 1) && (not (null (chainInputs chain))) = error $ "bad chain -- first chain shouldn't have inputs:\n" ++ (show chain)
  | (chainValue chain + chainLength chain > 25) && (not (null (chainOutputs chain))) = error $ "bad chain -- last chain shouldn't have outputs:\n" ++ (show chain)
  | otherwise = trace ("nascent chain: " ++ (show chain)) chain

{-
replaceThing :: Eq a => a -> a -> a -> a
replaceThing old new thing =
    if old == thing
        then new
        else thing
-}

replaceThing :: Eq a => a -> a -> a -> a
replaceThing old new thing = replaceThingGood (== old) new thing

replaceThingGood :: (a -> Bool) -> a -> a -> a
replaceThingGood qualifier replacement thing =
    if qualifier thing
        then replacement
        else thing

replaceChain :: CellID -> Chain -> Chain -> Chain
replaceChain chainID replacement chain =
    replaceThingGood (\c -> (cid c) == chainID) replacement chain

linkChains :: Chain -> Chain -> Chain
linkChains chain1 chain2 = trace ("linking " ++ cid1 ++ " with " ++ cid2) $ Chain {
    cid = cid1,
    chainCells = (chainCells chain1) ++ (chainCells chain2),
    chainValue = newValue,
    chainLength = (chainLength chain1) + (chainLength chain2),
    chainOutputs = filter (/= cid1) (chainOutputs chain2),
    chainInputs = filter (/= cid2) (chainInputs chain1)
}
    where newValue
            | value1 == 0 && value2 == 0 = 0
            | value1 == 0 = value2 - length1
            | value2 == 0 = value1
            | value1 == value2 - length1 = value1
            | otherwise = error $ "can't link chains from\n" ++ (show chain1) ++ "\nto\n" ++ (show chain2) ++ "\ndue to value mismatch"
          cid1 = cid chain1
          cid2 = cid chain2
          value1 = chainValue chain1
          length1 = chainLength chain1
          value2 = chainValue chain2

getChain :: CellID -> Board -> Chain
getChain chainID board = fromMaybe (error $ "can't find cid " ++ chainID) $ find (\chain -> (cid chain) == chainID) board

setChain :: CellID -> Chain -> Board -> Board
setChain chainID chain [] = []
setChain chainID chain (c:cs)
  | (cid c) == chainID = chain : cs
  | otherwise = c : (setChain chainID chain cs)

{-
reactSingleOutput :: Board -> Chain -> Maybe Action
reactSingleOutput board (chainID, cellIDs, value, length, outputs, inputs)
  | length outputs == 1
    | length tInputs > 1
      where (getChain board (first outputs))
      -}

{-
puzzle1 = Map.fromList [
    ("a1", ("a1" 0 ["a3"] ["b1"])),
    ("a2", ("a2" 1 ["b1"] [])),
    ("a3", ("a3" 0 ["b2"] ["a1"])),
    ("b1", ("b1" 0 ["a1"] ["a2"])),
    ("b2", ("b2" 0 ["b3"] ["a3"])),
    ("b3", ("b3" 6 [] ["b2"]))
    ]
    -}

puzzle3 = [
    ("a1", 1, ["a2", "a3"]),
    ("a2", 0, ["b3"]),
    ("a3", 0, ["b2", "c1"]),
    ("b1", 0, ["c1"]),
    ("b2", 0, ["c2"]),
    ("b3", 0, ["b2", "b1"]),
    ("c1", 0, ["c2", "c3"]),
    ("c2", 0, ["b2", "a2"]),
    ("c3", 9, [])
    ]

puzzle5 = [
    ("a1", 0, ["a2", "a3", "a4", "a5"]),
    ("a2", 0, ["b3", "c4", "d5"]),
    ("a3", 0, ["a4", "a5"]),
    ("a4", 0, ["b3", "c2", "d1"]),
    ("a5", 16, ["a4", "a3", "a2", "a1"]),
    
    ("b1", 0, ["b2", "b3", "b4", "b5"]),
    ("b2", 0, ["c1"]),
    ("b3", 0, ["c4", "d5"]),
    ("b4", 0, ["b5"]),
    ("b5", 0, ["c4", "d3", "e2"]),
    
    ("c1", 25, []),
    ("c2", 0, ["d3", "e4"]),
    ("c3", 1, ["d2", "e1"]),
    ("c4", 10, ["c5"]),
    ("c5", 0, ["d4", "e3"]),
    
    ("d1", 0, ["e1"]),
    ("d2", 0, ["c2", "b2", "a2"]),
    ("d3", 0, ["d4", "d5"]),
    ("d4", 0, ["b2", "a1"]),
    ("d5", 0, ["e4"]),

    ("e1", 0, ["e2", "e3", "e4", "e5"]),
    ("e2", 0, ["d2", "c2", "b2", "a2"]),
    ("e3", 12, ["e4", "e5"]),
    ("e4", 0, ["d3", "c2", "b1"]),
    ("e5", 0, ["d4", "b2", "a1"])
    ]

puzzleToBoard :: [(CellID, Int, [CellID])] -> Board
puzzleToBoard protoCells = map reify protoCells
    where reify (cellID, value, outputs) = Chain {
        cid = cellID, chainCells = [cellID],
        chainValue = value, chainLength = 1,
        chainOutputs = outputs, chainInputs = (inputs cellID)
    }
          inputs cellID = map (\(cellID, _, _) -> cellID) (filter (\(_, _, outputs) -> elem cellID outputs) protoCells)

boardToSolveState :: Board -> SolveState
boardToSolveState board = SolveState {
    stateManeuvers = Map.empty,
    statePossibleActions = [ Action { actionName = "noop", actionAction = id } ]
}
    
main = do
    let solution = solvePrintingly (boardToSolveState (puzzleToBoard puzzle5))
    putStrLn $ "signposts maneuvers: " ++ (show (length (stateManeuvers solution))) ++ ", actions: " ++ (show (length (statePossibleActions solution)))
    putStrLn $ "final solution: " ++ (show solution)
