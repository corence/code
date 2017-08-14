-- https://www.hackerrank.com/challenges/crush

-- imo this needs Vector https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html

-- Enter your code here. Read input from STDIN. Print output to STDOUT

type Index = Int
type Value = Integer
data Operation = Operation Index Index Value

main = do
    [numCells, numOperations] <- map read <$> words <$> getLine
    operations <- sequence $ replicate numOperations parseOperation
    pure ()
    
    
parseOperation :: IO Operation
parseOperation = do
    fragments <- words <$> getLine
    pure $ Operation (read $ fragments !! 0) (read $ fragments !! 1) (read $ fragments !! 2)
