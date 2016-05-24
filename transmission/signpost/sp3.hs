
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Solver
import Signpost
import SignpostPuzzles
import ListUtil

board :: Board
board = [
    Chain {cid = "a1", chainCells = ["a1","b2","b1","d1","b3","c4"], chainValue = 1, chainLength = 6, chainOutputs = ["d4","e4","f4"], chainInputs = []},
    Chain {cid = "a7", chainCells = ["a7","c7","g7"], chainValue = 47, chainLength = 3, chainOutputs = [], chainInputs = ["d4","e3","f2"]},
    Chain {cid = "b4", chainCells = ["b4"], chainValue = 0, chainLength = 1, chainOutputs = ["c3","d2","e1"], chainInputs = ["d3","f4","e2"]},
    Chain {cid = "b6", chainCells = ["b6"], chainValue = 0, chainLength = 1, chainOutputs = ["c6","d6","e6","f6"], chainInputs = ["c5","d4","e3","e6","f2","f6"]},
    Chain {cid = "b7", chainCells = ["b7"], chainValue = 0, chainLength = 1, chainOutputs = ["d7","e7","f7"], chainInputs = ["a7","c5","f3"]},
    Chain {cid = "c3", chainCells = ["c3"], chainValue = 0, chainLength = 1, chainOutputs = ["d4","e5","f6"], chainInputs = ["a1","d5","d3","b4","c5","e5"]},
    Chain {cid = "c5", chainCells = ["c5","c1","g1","g2","f1","b5"], chainValue = 26, chainLength = 6, chainOutputs = ["b6","b7"], chainInputs = ["d4","e3","f2","g3"]},
    Chain {cid = "c6", chainCells = ["c6"], chainValue = 0, chainLength = 1, chainOutputs = ["d6","e6","f6"], chainInputs = ["b6","e6","f3","f6"]},
    Chain {cid = "d2", chainCells = ["d2"], chainValue = 0, chainLength = 1, chainOutputs = ["e3","f4"], chainInputs = ["d3","b4","d6"]},
    Chain {cid = "d3", chainCells = ["d3","c2","a4","a5"], chainValue = 40, chainLength = 4, chainOutputs = ["b4","c3","d2","e1"], chainInputs = ["d6","f5"]},
    Chain {cid = "d4", chainCells = ["d4"], chainValue = 0, chainLength = 1, chainOutputs = ["c5","b6","a7"], chainInputs = ["a1","c3","a1","d6","e3","e5","f2","f4","e2"]},
    Chain {cid = "d5", chainCells = ["d5","a2","a6","a3"], chainValue = 19, chainLength = 4, chainOutputs = ["c3","e3","g3"], chainInputs = ["d6","g3"]},
    Chain {cid = "d6", chainCells = ["d6"], chainValue = 0, chainLength = 1, chainOutputs = ["d5","d4","d3","d2"], chainInputs = ["b6","c6","e6","f6"]},
    Chain {cid = "d7", chainCells = ["d7"], chainValue = 0, chainLength = 1, chainOutputs = ["e7","f7"], chainInputs = ["a7","b7","a7"]},
    Chain {cid = "e1", chainCells = ["e1"], chainValue = 0, chainLength = 1, chainOutputs = ["f2","g3"], chainInputs = ["d3","a1","b4","c5","e4","e7"]},
    Chain {cid = "e2", chainCells = ["e2","g4"], chainValue = 22, chainLength = 2, chainOutputs = ["f4","e4","d4","b4"], chainInputs = ["e4","e7"]},
    Chain {cid = "e3", chainCells = ["e3"], chainValue = 0, chainLength = 1, chainOutputs = ["d4","c5","b6","a7"], chainInputs = ["d5","d2","e4","e7","f2"]},
    Chain {cid = "e4", chainCells = ["e4"], chainValue = 0, chainLength = 1, chainOutputs = ["e3","e2","e1"], chainInputs = ["a1","e7","f3","f4","e2","f5"]},
    Chain {cid = "e5", chainCells = ["e5"], chainValue = 3, chainLength = 1, chainOutputs = ["d4","c3"], chainInputs = ["c3","e7","g3"]},
    Chain {cid = "e6", chainCells = ["e6"], chainValue = 0, chainLength = 1, chainOutputs = ["d6","c6","b6"], chainInputs = ["a1","b6","c6","e7","f6"]},
    Chain {cid = "e7", chainCells = ["e7"], chainValue = 0, chainLength = 1, chainOutputs = ["e6","e5","e4","e3","e2","e1"], chainInputs = ["a7","b7","a7","d7"]},
    Chain {cid = "f2", chainCells = ["f2"], chainValue = 0, chainLength = 1, chainOutputs = ["e3","d4","c5","b6","a7"], chainInputs = ["e1","f7"]},
    Chain {cid = "f3", chainCells = ["f3"], chainValue = 16, chainLength = 1, chainOutputs = ["e4","c6","b7"], chainInputs = ["f7"]},
    Chain {cid = "f4", chainCells = ["f4"], chainValue = 0, chainLength = 1, chainOutputs = ["e4","d4","b4"], chainInputs = ["a1","d2","f7","e2"]},
    Chain {cid = "f5", chainCells = ["f5","g6"], chainValue = 0, chainLength = 2, chainOutputs = ["e4","d3"], chainInputs = ["f7","g3"]},
    Chain {cid = "f6", chainCells = ["f6"], chainValue = 11, chainLength = 1, chainOutputs = ["e6","d6","c6","b6"], chainInputs = ["b6","c3","c6","f7"]},
    Chain {cid = "f7", chainCells = ["f7"], chainValue = 0, chainLength = 1, chainOutputs = ["f6","f5","f4","f3","f2"], chainInputs = ["a7","a1","b7","a7","d7"]},
    Chain {cid = "g3", chainCells = ["g3","g5"], chainValue = 0, chainLength = 2, chainOutputs = ["f5","e5","d5","c5"], chainInputs = ["d5","e1","c5"]}
    ]

main = do
    putStrLn $ formatList board
    putStrLn $ "------"
    let idAction = Action { actionName = "id", actionTransformer = id, actionBoard = board }
    let (newActions, newBoard) = resolveAction idAction board
    
    putStrLn $ formatList newBoard
    putStrLn $ "======"
    putStrLn $ formatList newActions
