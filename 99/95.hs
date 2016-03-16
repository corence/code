
wordify95 :: Int -> String
--wordify95 n = implode95 "-" (explodeDigits95 10 n >>= wordifyDigit95)
wordify95 n = implode95 "-" words
    where digits = explodeDigits95 10 n
          words = map wordifyDigit95 digits
--wordify95 n = (map wordifyDigit95) . (explodeDigits95 10) n

wordifyDigit95 :: Int -> String
wordifyDigit95 0 = "zero"
wordifyDigit95 1 = "one"
wordifyDigit95 2 = "two"
wordifyDigit95 5 = "five"
wordifyDigit95 7 = "seven"

implode95 :: String -> [String] -> String
implode95 _ [s] = s
implode95 separator (s:ss) = s ++ separator ++ implode95 separator ss

explodeDigits95 :: Int -> Int -> [Int]
explodeDigits95 radix n
  | n < radix = [n]
  | otherwise = (explodeDigits95 radix (n `div` radix)) ++ [(n `mod` radix)]

runProblem :: (Eq t, Show t) => String -> t -> t -> IO ()
runProblem pid expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ pid ++ ": [" ++ show expected ++ "] != [" ++ show actual ++ "]"

main = do
  runProblem "95a" [2,4,5] $ explodeDigits95 10 245
  runProblem "95b" [1,0,1] $ explodeDigits95 2 5
  runProblem "95c" "one-seven-five" $ wordify95 175
