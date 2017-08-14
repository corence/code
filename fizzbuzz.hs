
import Data.List
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
    -- first solution
    putStrLn $ intercalate " " $ map fizzbuzz [1..100]

    -- second solution
    putStrLn $ intercalate " " $ map (fizz buzzes) [1..100]
    where buzzes = [(buzz 3 "fizz"), (buzz 5 "buzz")]

-- first solution
fizzbuzz :: Int -> String
fizzbuzz n
    | match3 && match5 = "fizzbuzz"
    | match3 = "fizz"
    | match5 = "buzz"
    | otherwise = show n
    where match3 = n `mod` 3 == 0
          match5 = n `mod` 5 == 0

-- second solution follows --

type Buzz = Int -> Maybe String

-- Given a set of "Buzz" conditions, apply them all to a number and return the String that should be printed for that number
fizz :: [Buzz] -> Int -> String
fizz buzzes input
  | null outputs = show input
  | otherwise = join outputs
  where outputs = doBuzzes buzzes input

doBuzzes :: [Buzz] -> Int -> [String]
doBuzzes buzzes input = catMaybes $ map (\buzz -> buzz input) buzzes

-- Returns the String if the last arg is a multiple of the divisor, otherwise Nothing
buzz :: Int -> String -> Buzz
buzz divisor output input =
    if input `mod` divisor == 0
        then Just output
        else Nothing
