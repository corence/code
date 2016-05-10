
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Solver
import Signpost

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
