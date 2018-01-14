
module BoardParser where

import Text.ParserCombinators.ReadP
import Board
import Control.Lens.Operators

-- format is like this:
-- 10x10:2/3.2/1.1.3/2.5/2.2.4/1.1.4/1.3/1.1.2/4/4.1/5.2/2.3.3/1.2/6/3/1/4/6/7/4.1
-- or this. 2 rows, 3 cols. Col hints are first.
-- 3x2:2/1//2/1

parseBoard :: ReadP Board
parseBoard = do
    dimensions <- parseDimensions
    char ':'
    hints <- parseBoardHints dimensions
    skipSpaces
    eof
    return $ Board dimensions hints

parseInt :: ReadP Int
parseInt = many1 (choice digits) <&> read
    where digits = map char ['0'..'9']

parseDimensions :: ReadP Dimensions
parseDimensions = do
    numCols <- parseInt
    char 'x'
    numRows <- parseInt
    return (numCols, numRows)

parseBoardHints :: Dimensions -> ReadP BoardHints
parseBoardHints (numCols, numRows) = do
    allBoardHints <- sepBy parseShaftHints (char '/')
    let colHints = take numCols allBoardHints
    let rowHints = drop numCols allBoardHints
    return (colHints, rowHints)

parseShaftHints :: ReadP ShaftHints
parseShaftHints = do
    sepBy parseInt (char '.')
