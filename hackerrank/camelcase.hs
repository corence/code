
main :: IO ()
main = do
    s <- getLine
    let numWords = 1 + (length . filter (>= 'A') . filter (<= 'Z')) s
    putStrLn $ show $ numWords
