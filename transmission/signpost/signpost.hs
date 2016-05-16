
module Signpost
( Board
, Chain(Chain, cid, chainCells, chainValue, chainLength, chainOutputs, chainInputs)
, CellID
, Action(Action, actionName, actionTransformer, actionBoard)
, getChain
, linkChains
, replaceChainReferences
, replaceChain
, linkChainsInBoard
, verifyBoard
) where

import Data.Maybe
import Data.List
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
    else cell : removeChain chainID cells

-- 1) remove chain1 and chain2
-- 2) replace chain1's outputs with those from chain2
-- 3) replace any inputs that refer to chain2, with chain1
-- 4) any inputs or outputs that now have a value mismatch (eg if a 7 lists a 10 in its outputs) should disconnect
linkChainsInBoard :: CellID -> CellID -> Board -> Board
linkChainsInBoard chain1ID chain2ID board = newBoard
    --where newBoard = disconnectValueMismatches chain1ID $ replaceReferences chain2ID chain1ID $ replaceLinkedChains $ board
    where newBoard
                    = (\b -> trace ("4. associated " ++ formatList b) b)
                    $ associateValueMatches chain1ID
                    $ (\b -> trace ("3. disassociated " ++ formatList b) b)
                    $ disassociateValueMismatches chain1ID
                    $ (\b -> trace ("2. re-referenced " ++ formatList b) b)
                    $ replaceReferences chain2ID chain1ID
                    $ (\b -> trace ("1. linked " ++ formatList b) b)
                    $ replaceLinkedChains
                    $ (\b -> trace ("0. linkChainsInBoard " ++ chain1ID ++ " " ++ chain2ID ++ formatList b) b)
                    $ board 
          replaceLinkedChains board = replaceChain chain1ID newChain (removeChain chain2ID board)
              where chain1 = getChain chain1ID board
                    chain2 = getChain chain2ID board
                    newChain = linkChains chain1 chain2
          replaceReferences fromID toID board = map (\chain -> chain {
                                                    chainInputs = map (replaceThing fromID toID) (chainInputs chain),
                                                    chainOutputs = filter (/= fromID) (chainOutputs chain)
                                                  }) board

verifyBoard :: Board -> Board
verifyBoard board = map (verifyChain board) board

verifyChain :: Board -> Chain -> Chain
verifyChain board chain
  | (chainValue chain == 1) && (not (null (chainInputs chain))) = error $ "bad chain -- first chain shouldn't have inputs:\n" ++ (show chain)
  | (chainValue chain + chainLength chain > (countCells board)) && (not (null (chainOutputs chain))) = error $ "bad chain -- last chain shouldn't have outputs:\n" ++ (show chain)
  | not (null mismatchedOutputs) = error $ "bad chain -- outputs " ++ show mismatchedOutputs ++ " don't value-match:\n" ++ (show chain)
  | not (null mismatchedInputs) = error $ "bad chain -- inputs " ++ show mismatchedInputs ++ " don't value-match:\n" ++ (show chain)
  | otherwise = trace ("nascent chain: " ++ (show chain)) chain
  where mismatchedOutputs = filter (\outputID -> not $ couldLinkValuesMatch chain (getChain outputID board)) (chainOutputs chain) 
        mismatchedInputs = filter (\inputID -> not $ couldLinkValuesMatch (getChain inputID board) chain) (chainInputs chain) 

countCells :: Board -> Int
countCells = foldr (\chain -> (+ (chainLength chain))) 0

disassociateValueMismatches :: CellID -> Board -> Board
disassociateValueMismatches chainID board = foldr (\(source, target) b -> disassociateChainsInBoard (cid source) (cid target) b) board mismatches
  where potentials = (map (\outputID -> (chain, getChain outputID board)) (chainOutputs chain)) ++ (map (\inputID -> (getChain inputID board, chain)) (chainInputs chain))
        mismatches = filter (\(source, target) -> not $ couldLinkValuesMatch source target) potentials
        chain = getChain chainID board

associateValueMatches :: CellID -> Board -> Board
associateValueMatches chainID board = foldr (\(source, target) b -> linkChainsInBoard (cid source) (cid target) b) board matches
  where potentials = (map (\outputID -> (chain, getChain outputID board)) (chainOutputs chain)) ++ (map (\inputID -> (getChain inputID board, chain)) (chainInputs chain))
        matches = filter (\(source, target) -> doLinkValuesMatchStrict source target) potentials
        chain = getChain chainID board

unlinkValueMismatchesComment :: CellID -> Board -> Board
unlinkValueMismatchesComment chainID board = replaceChain chainID newChain board
    where newChain = chain {
                            chainInputs = filter (\inputID -> couldLinkValuesMatch (getChain inputID board) chain) (chainInputs chain),
                            chainOutputs = filter (\outputID -> couldLinkValuesMatch chain (getChain outputID board)) (chainOutputs chain)
                        }
          chain = getChain chainID board
          
couldLinkValuesMatch :: Chain -> Chain -> Bool
couldLinkValuesMatch chain1 chain2
  | (chainValue chain1 == 0) || (chainValue chain2 == 0) = True
  | otherwise = doLinkValuesMatchStrict chain1 chain2
  
doLinkValuesMatchStrict :: Chain -> Chain -> Bool
doLinkValuesMatchStrict chain1 chain2
  | (chainValue chain1 + chainLength chain1) == (chainValue chain2) = True
  | otherwise = False
  
disassociateChainsInBoard :: CellID -> CellID -> Board -> Board
disassociateChainsInBoard chain1ID chain2ID board = replaceChain chain1ID newChain1 (replaceChain chain2ID newChain2 board)
    where newChain1 = chain1 { chainOutputs = filter (/= chain2ID) (chainOutputs chain1) }
          newChain2 = chain2 { chainInputs = filter (/= chain1ID) (chainInputs chain2) }
          chain1 = getChain chain1ID board
          chain2 = getChain chain2ID board
