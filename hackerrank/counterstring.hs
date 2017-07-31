
import System.Environment 

counterstring :: String -> Int -> String
counterstring filler count
  | count <= 0 = []
  | otherwise = counterstring filler (count - length shavedSubstring) ++ shavedSubstring
    where substring = show count ++ filler
          shavedSubstring = drop (length substring - count) substring -- note: if "drop" gets a negative number, it returns the whole list unmodified
    
--main = (counterstring '.' <$> read <$> head <$> getArgs) >>= putStrLn
main = do
    args <- getArgs
    case args of
        [] -> putStrLn $ "args: 1) a number indicating desired length; 2) string to fill in with -- defaults to: \".\""
        (count:[]) -> putStrLn $ counterstring "." (read count)
        (count:filler:_) -> putStrLn $ counterstring filler (read count)

-- testing follows 

expectedValues = [
    ".",
    "2.",
    ".3.",
    "2.4.",
    ".3.5.",
    "2.4.6.",
    ".3.5.7.",
    "2.4.6.8.",
    ".3.5.7.9.",
    ".3.5.7.10.",
    "2.4.6.8.11.",
    ".3.5.7.9.12.",
    ".3.5.7.10.13."
    ]

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y
  | x == y = putStrLn "ok!"
  | otherwise = putStrLn $ "no match: " ++ show x ++ " / " ++ show y

test = sequence_ $ map (\expected -> assertEqual expected (counterstring "." (length expected))) expectedValues
