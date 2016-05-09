
attempts = [ ("rrrryyyy", 1, 3), ("ggbbggbb", 2, 2), ("ovvoovvo", 0, 2), ("cxcxcxcx", 0, 1), ("xoorrrbb", 0, 3), ("gyxygooy", 4, 6), ("rygyxgov", 3, 6), ("rgyygooc", 2, 4)]

generate _ 0 = []
generate options length = foldr (\sublist results -> addEachOption sublist results options) [] sublists
    where sublists = generate options (length-1)
          addEachOption sublist = foldr (\option results -> (option : sublist) : results)

validGuess [] _ = True
validGuess (attempt:attempts) guess
  | (positionCorrect < attemptPosition) || (colorCorrect < attemptColor) = False
  | otherwise = validGuess attempts guess
    where (_, positionCorrect, colorCorrect) = matchAttempt attemptBoard guess
          (attemptBoard, attemptPosition, attemptColor) = attempt

matchAttempt a b = (b, max 0 positionCorrect, max 0 colorCorrect)
    -- needs a zip not a foldr
    --where positionCorrect = foldr matchCell 0 (a, b)
          --matchCell [] [] = (0, 0)

main = do
    let allPossibleGuesses = generate "rygbovxc" 8
    let filteredGuesses = filter (validGuess attempts) allPossibleGuesses
    putStrLn $ (show (length filteredGuesses))
