
import Data.Map

type Pos = (Int, Int)

poses :: [Pos]
poses = do
    x <- [1..9]
    y <- [1..9]
    pure (x, y)

type Value = Int
type Board = Map Pos Value

isSolved :: Board -> Bool
isSolved = (>= 81 . size)

solve :: Board -> Board
solve board = foldr solvePos board poses
    where solvePos pos board
              = if Map.member pos board
                    then board
                    else 
