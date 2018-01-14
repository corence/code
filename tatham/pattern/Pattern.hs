
-- following along http://maweki.de/tech/solving-tathams-puzzles-pattern.html

import Board
import BoardParser
import Control.Applicative
import Control.Lens.Operators
import Control.Monad.State.Class
import qualified Data.Map as Map
import Data.Map(Map(..))
import Ersatz
import Prelude hiding (all, any, not, (&&))

-- terms:
-- "shaft" -- all of the hints for a row or column
-- "cell" -- one square in the grid; can be black white or grey
-- "stream" -- a sequence of black squares in a row or column
-- "board" -- all of the cells and shafts

main = pure ()

-- i know these are unnecessary but i find them so much more readable
allTrue = all id
allFalse = all not

valid :: Board -> Bool
valid (Board (numCols, numRows) (colHints, rowHints)) =
    numRows == length colHints
    && numCols == length rowHints
    && all (isHintLengthCorrect numCols) rowHints
    && all (isHintLengthCorrect numRows) colHints
    where isHintLengthCorrect shaftLength shaftHints
            = sum shaftHints + (length shaftHints - 1) <= shaftLength

processShaft :: [Int] -> [Bit] -> Bit
processShaft [] cells = allFalse cells
processShaft [0] cells = allFalse cells
processShaft (streamLength:streamLengths) cells
  = map buildStream [0..maxPrecedingBlanks] & any id
    where maxPrecedingBlanks = length cells - minLength streamLengths - streamLength
          walledCells = [false] ++ cells ++ [false] -- we insert fake empty cells ("walls") at the start and end, so that we can say that all streams need walls
          streamMatcher = [not] ++ replicate streamLength id ++ [not]
          minLength stream = length stream + sum stream
          buildStream numPrecedingBlanks
              = allFalse (take numPrecedingBlanks cells)
              && allTrue (zipWith ($) streamMatcher (drop numPrecedingBlanks walledCells))
              && processShaft streamLengths (drop (streamLength + numPrecedingBlanks + 1) cells)

processBoard :: (HasSAT s, Control.Monad.State.Class.MonadState s m) => Board -> m ()
processBoard board@(Board (numCols, numRows) (colHints, rowHints)) =
    makeBlankCells (numCols, numRows) <&> assert (allTrue bits)
    where bits = makeBits board

makeBits :: Board -> [Bit]
makeBits (Board (numCols, numRows) (colHints, rowHints))
    = shaftBits colHints rowKeys ++ shaftBits rowHints colKeys
        where shaftBits streams getKeys = (map getKeys [0..]) & zipWith processShaft streams
              colKeys rowIndex = [0..(numRows-1)] <&> map (\rowIndex -> (rowIndex, colIndex))
              rowKeys rowIndex = [0..(numCols-1)] <&> map (\colIndex -> (rowIndex, colIndex))

makeBlankCells :: (HasSAT s, Control.Monad.State.Class.MonadState s m) => (Int, Int) -> m (Map (Int, Int) Bit)
makeBlankCells (numCols, numRows) =
    let allIndices = liftA2 (,) [0..numCols] [0..numRows]
        cells = replicateM (length allIndices) exists
    in cells <&> zip allIndices <&> Map.fromList

{-
nono :: (MonadState s m, HasSAT s) => Problem -> m (Map (Int,Int) Bit)
nono (Problem (sizex, sizey) (xin, yin)) = do
  let indices = [(x, y) | x <- [1..sizex], y <- [1..sizey] ]
  vars' <- replicateM (length indices) exists
  vars <- return $ Map.fromList $ zip indices vars'
  let getx x = for [1..sizey] $ \y -> (vars ! (x,y))
      gety y = for [1..sizex] $ \x -> (vars ! (x,y))
      xs = zipWith build_runs xin (map getx [1..])
      ys = zipWith build_runs yin (map gety [1..])
  assert (all id xs && all id ys)
  return vars
  -}
