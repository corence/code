{-
Define a recursive function
    split :: Char -> String -> [String]
that breaks a string into substrings delimited by a given character. Some usage examples:
    split '@' "pbv@dcc.fc.up.pt" = ["pbv","dcc.fc.up.pt"]
    split '/' "/usr/include" = ["", "usr", "include"]
Suggestion: use the takeWhile and dropWhile functions from the standard Prelude.
-}

import Test.QuickCheck

split :: Char -> String -> [String]
split _ [] = [""]
split c (x:xs)
  | c == x = "" : (r:rs)
  | otherwise = (x : r) : rs
    where (r:rs) = split c xs

prop_num_strings :: Char -> String -> Bool
prop_num_strings c s = length (split c s) == 1 + length (filter (== c) s)

main = quickCheck prop_num_strings
