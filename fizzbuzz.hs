
import Data.List

main :: IO ()
main = putStrLn $ intercalate " " $ map fizzbuzz [1..100]

fizzbuzz :: Int -> String
fizzbuzz n
    | match3 && match5 = "fizzbuzz"
    | match3 = "fizz"
    | match5 = "buzz"
    | otherwise = show n
    where match3 = n `mod` 3 == 0
          match5 = n `mod` 5 == 0
