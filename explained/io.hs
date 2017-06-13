
import System.Directory

firstLine :: String -> IO String
firstLine filename = readFile filename >>= pure . lines >>= pure . head

main = main0

main0 = getCurrentDirectory >>= listDirectory >>= (\filename -> putStrLn $ filename ++ ": " >> firstLine filename >>= putStrLn)

-- ">>=" style 1, with pure
main1 :: IO ()
main1 =
    putStr "Current directory is: " >>
    getCurrentDirectory >>= putStrLn >>
    putStrLn "Give me a filename and I'll print the first line..." >>
    getLine >>= readFile >>= pure . lines >>= pure . head >>= putStrLn



-- ">>=" style 2, without pure
main2 :: IO ()
main2 =
    putStrLn "Give me a filename and I'll print the first line..." >>
    getLine >>=
    readFile >>= (\fileContents ->
    putStrLn (head (lines fileContents)))

-- ">>=" style 3, with let
main3 :: IO ()
main3 =
    putStrLn "Give me a filename and I'll print the first line..." >>
    getLine >>=
    readFile >>= (\fileContents ->
        let fileLines = lines fileContents
            firstLine = head fileLines
        in putStrLn firstLine)

-- ">>=" style 4, with where
main4 :: IO ()
main4 =
    putStrLn "Give me a filename and I'll print the first line..." >>
    getLine >>=
    readFile >>= (\fileContents ->
        let fileLines = lines fileContents
            firstLine = head fileLines
        in putStrLn firstLine)
