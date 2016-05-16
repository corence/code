
module ListUtil
( formatList
, replaceThing
, replaceThingGood
, convergeMaybes
, trace
) where

--import Debug.Trace
trace _ = id

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

convergeMaybes :: [[Maybe a]] -> [a]
convergeMaybes maybeList = foldr (\maybes justs -> prependMaybes justs maybes) [] maybeList

prependMaybes :: [a] -> [Maybe a] -> [a]
prependMaybes justs maybes = foldr addIfHasValue justs maybes
    where addIfHasValue :: Maybe b -> [b] -> [b]
          addIfHasValue possibleValue justies = maybe justies (prepend justies) possibleValue

prepend :: [a] -> a -> [a]
prepend list element = element : list

times :: Int -> (a -> a) -> a -> a
times 0 _ item = item
times num func item = times (pred num) func (func item)
