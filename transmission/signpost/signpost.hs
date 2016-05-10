
module Signpost
( Board
, Chain(cid, chainCells, chainValue, chainLength, chainOutputs, chainInputs)
, CellID
, getChain
, linkChains
, replaceChainReferences
, replaceChain
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

replaceChain :: CellID -> Chain -> Chain -> Chain
replaceChain chainID replacement chain =
    replaceThingGood (\c -> (cid c) == chainID) replacement chain

