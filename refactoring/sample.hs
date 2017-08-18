
module Sample where

deleteFromList :: [a] -> Int -> [a]
deleteFromList (x:xs) index = if index > 0 then deleteFromList xs (index - 1) else xs
