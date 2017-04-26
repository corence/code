import Test.QuickCheck

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

--main = quickCheck prop_revapp

{-
Define a recursive function split :: Char -> String -> [String] that breaks a string into substrings delimited by a given character. Some usage examples:
split '@' "pbv@dcc.fc.up.pt" = ["pbv","dcc.fc.up.pt"]
split '/' "/usr/include" = ["", "usr", "include"]
Suggestion: use the takeWhile and dropWhile functions from the standard Prelude.
-}
split :: Char -> String -> [String]
--split _ [] = []
split _ [] = [""]
split delimiter (c:cs) = if c == delimiter
                             then [] : split delimiter cs
                             else case split delimiter cs of
                                 (r : rs) -> (c : r) : rs
                                 [] -> [[c]]

--split delimiter string = takeWhile (== delimiter) string : split delimiter (tail $ dropWhile (/= delimiter) string)
--split2 delimiter (c:cs) results = if delimiter == c
                                      --then split2 delimiter cs

{-
We can devise a property for split by consider the inverse function of split which I shall call  unsplit. Here are some examples of what we want unsplit to do:

unsplit '@' ["pbv", "dcc.fc.up.pt"] = "pbv@dcc.fc.up.pt"
unsplit '/' ["", "usr", "include"] = "/usr/include"
-}
unsplit :: Char -> [String] -> String
unsplit _ [] = ""
unsplit _ (s:[]) = s
unsplit delimiter (s:ss) = s ++ (delimiter : (unsplit delimiter ss))

--prop_split_inv c xs = unsplit c (split c xs) == xs
{-
prop_split_inv c xs 
    = let ys = split c xs in 
      collect (length ys) $ unsplit c ys == xs
-}
prop_split_inv xs
    = forAll (elements xs) $ \c -> 
      unsplit c (split c xs) == xs

main = quickCheck prop_split_inv
