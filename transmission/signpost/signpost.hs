
module Signpost
( Board
, Chain(Chain, cid, chainCells, chainValue, chainLength, chainOutputs, chainInputs)
, CellID
, Action(Action, actionName, actionTransformer, actionBoard)
, getChain
, linkChains
, replaceChainReferences
, replaceChain
, replaceLinkChains
) where

import Data.Maybe
import Data.List
import Debug.Trace
import ListUtil

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

data Action = Action {
    actionName :: String,
    actionTransformer :: Board -> Board,
    actionBoard :: Board
}

instance Show Action where
    show action = actionName action


getChain :: CellID -> Board -> Chain
getChain chainID board = fromMaybe (error $ "can't find cid " ++ chainID) $ find (\chain -> (cid chain) == chainID) board

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

-- replace references from the old chain to the new chain
replaceChainReferences :: CellID -> CellID -> Chain -> Chain
replaceChainReferences chain1ID chain2ID chain = chain {
    chainInputs = map (replaceThing chain2ID chain1ID) (chainInputs chain),
    chainOutputs = filter (/= chain2ID) (chainOutputs chain)
}

replaceChain :: CellID -> Chain -> Board -> Board
replaceChain chainID replacement board =
    map (replaceThingGood (\c -> (cid c) == chainID) replacement) board

removeChain :: CellID -> Board -> Board
removeChain chainID [] = error $ "tried to remove " ++ chainID ++ " from a board that didn't have it"
removeChain chainID (cell:cells) = if (cid cell) == chainID
    then cells
    else removeChain chainID cells

-- 1) remove chain1 and chain2
-- 2) replace chain1's outputs with those from chain2
-- 3) replace any inputs that refer to chain2, with chain1
-- 4) any inputs or outputs that now have a value mismatch (eg if a 7 lists a 10 in its outputs) should disconnect
linkChainsInBoard :: CellID -> CellID -> Board -> Board
linkChainsInBoard chain1ID chain2ID board = newBoard
    --where newBoard = disconnectValueMismatches chain1ID $ replaceReferences chain2ID chain1ID $ replaceLinkedChains $ board
    where newBoard
                    = (\b -> trace ("3. disconnected " ++ formatList b) b)
                    $ disconnectValueMismatches chain1ID
                    $ (\b -> trace ("2. re-referenced " ++ formatList b) b)
                    $ replaceReferences chain2ID chain1ID
                    $ (\b -> trace ("1. linked " ++ formatList b) b)
                    $ replaceLinkedChains
                    $ board 
          replaceLinkedChains board = replaceChain chain1ID newChain (removeChain chain2ID board)
              where chain1 = getChain chain1ID board
                    chain2 = getChain chain2ID board
                    newChain = linkChains chain1 chain2
          replaceReferences fromID toID board = map (\chain -> chain {
                                                    chainInputs = map (replaceThing fromID toID) (chainInputs chain),
                                                    chainOutputs = filter (/= fromID) (chainOutputs chain)
                                                  }) board

comment chain1ID chain2ID board = disconnectValueMismatches chain1ID board
    where chain1 = getChain chain1ID board
          boardWithout1 = filter (\chain -> (cid chain) /= chain1ID) board
          otherChains = filter (\chain -> (cid chain) /= chain2ID) boardWithout1

          renewedChains = map (renewInputs . renewOutputs) otherChains
              where renewInputs chain = chain { chainInputs = map (replaceThing chain2ID chain1ID) (chainInputs chain) }
                    renewOutputs chain = chain { chainOutputs = filter (/= chain2ID) (chainOutputs chain) }

replaceLinkChains :: CellID -> CellID -> Board -> Board
replaceLinkChains chain1ID chain2ID board =
        trace ("adding chain " ++ show newChain ++ " to remainder " ++ show remainder) $ newChain : remainder
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
          dropOutputs chainID outputID board1 = traceShowId $ replaceChain chainID chain { chainOutputs = filter (/= outputID) (chainOutputs chain) } board1
          dropInputs chainID inputID board1 = traceShowId $ replaceChain chainID chain { chainInputs = filter (/= inputID) (chainInputs chain) } board1
          chain = trace ("getting da chain " ++ chainID ++ " from board " ++ (formatList board)) $ getChain chainID board

valueMismatch :: Chain -> Chain -> Bool
valueMismatch chain1 chain2
  | (chainValue chain1 == 0) || (chainValue chain2 == 0) = False
  | (chainValue chain1 + chainLength chain1) /= (chainValue chain2) = False
  | otherwise = True

