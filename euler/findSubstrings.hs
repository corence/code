{-
You have two arrays of strings, words and parts. Return an array that contains the strings from words, modified so that any occurrences of the substrings from parts are surrounded by square brackets [], following these guidelines:

If several parts substrings occur in one string in words, choose the longest one. If there is still more than one such part, then choose the one that appears first in the string.

Example
For words = ["Apple", "Melon", "Orange", "Watermelon"] and parts = ["a", "mel", "lon", "el", "An"], the output should be
findSubstrings(words, parts) = ["Apple", "Me[lon]", "Or[a]nge", "Water[mel]on"].
-}

import Data.List

findSubstrings :: [String] -> [String] -> [String]
findSubstrings words parts = map (markSubstrings parts) words

markSubstrings :: [String] -> String -> String
markSubstrings parts word
  = case sortedMatches of
        [] -> word
        (match:_) -> markSubstrings parts (newWord match)
        where matches = map (\part -> breakList part word) parts
              newWord (prefix, match, postfix) = prefix ++ "[" ++ match ++ "]" ++ postfix
              sortedMatches = sortBy (\left right -> compare (length left) (length right)) (concat matches)

{-
-- given a pattern and a list, return the parts of list that occur before and after the pattern -- or Nothing if the pattern doesn't appear
breakList :: [a] -> [a] -> [([a], [a])]
breakList _ [] = []
breakList pattern list = if isPrefixOf pattern list
                             then ([], drop (length pattern) list) : others
                             else others
                             where othersBase = breakList pattern (tail list)
                                   others = map (\(x, y) -> (head list : x, y) othersBase
-}

-- ew that sucks.
-- check this instead:
findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

subListIndices :: Eq a => [a] -> [a] -> [Int]
subListIndices pattern list = findIndices (isPrefixOf pattern) (tails list)

breakList :: Eq a => [a] -> [a] -> [([a], [a], [a])]
breakList pattern list = map (\index -> (take index list, pattern, drop (index + length pattern) list)) $ subListIndices pattern list

main = return ()
