
module Board where

type Dimensions = (Int, Int) -- numCols, numRows
type ShaftHints = [Int] -- the numeric hints for either a row or a column
type BoardHints = ([ShaftHints], [ShaftHints]) -- hints for cols, then hints for rows

data Board = Board Dimensions BoardHints deriving Show

