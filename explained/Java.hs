
module Java where

import System.Random

helloworld :: IO ()
helloworld = do {
    putStrLn ("Who are you?");
    name <- getLine :: IO String;
    putStrLn ("Hello " ++ name ++ "!");
    number <- randomIO :: IO Double;
    numBottles <- pure (floor (number * 8)) :: IO Integer;
    putStrLn ("There are " ++ show numBottles ++ " bottles of beer on the wall.");
}

main = helloworld
