import Control.Lens.Lens
import Control.Monad

{-
10 0 0 2 1
2
0 1 YES
3 4 NO
-}

{-
10 0 0 2 1
5
0 1 NO
1 2 NO
2 3 NO
3 4 YES
4 5 YES
-}

main = do
    numBalls <- getLine <&> words <&> head <&> read :: IO Int
    numQueries <- getLine <&> read :: IO Int
    if numQueries * 2 < numBalls
        then putStrLn (nextQuery numQueries)
        else do
            colors <- queryResults numQueries
            let solution = solve colors
            putStrLn $ show $ colors
            putStrLn $ show $ solution

data Action = Swap | Stay deriving (Show, Eq)
data Color = Black | White | Grey deriving (Show, Eq)
queryResults :: Int -> IO [Color]
queryResults numQueries = swaps <&> colors White <&> (White :)
    where swaps = replicateM numQueries (getLine <&> words <&> (!! 2) <&> (== "YES") <&> (\result -> if result then Stay else Swap))
          colors _ [] = []
          colors White (Stay:rs) = White : colors White rs
          colors White (Swap:rs) = Black : colors Black rs
          colors Black (Stay:rs) = Black : colors Black rs
          colors Black (Swap:rs) = White : colors White rs

solve :: [Color] -> Int
solve colors
  = if isMostlyWhite colors
        then 0
        else index Black colors
  where isMostlyWhite colors = length (filter (== White) colors) * 2 >= length colors

nextQuery :: Int -> String
nextQuery n = show n ++ " " ++ show (n + 1)

index :: Eq a => a -> [a] -> Int
index _ [] = error "couldn't find it"
index a (x:xs)
  | a == x = 0
  | otherwise = 1 + index a xs
