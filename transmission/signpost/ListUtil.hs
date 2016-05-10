
module ListUtil
( formatList
, replaceThing
, replaceThingGood
) where

formatList :: Show a => [a] -> String
formatList [] = "\n"
formatList (x:xs) = "\n" ++ (show x) ++ formatList xs

replaceThing :: Eq a => a -> a -> a -> a
replaceThing old new thing = replaceThingGood (== old) new thing

replaceThingGood :: (a -> Bool) -> a -> a -> a
replaceThingGood qualifier replacement thing =
    if qualifier thing
        then replacement
        else thing

