import Test.QuickCheck
import Data.List (intersperse)

split :: Char -> String -> [String]
split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]

-- show
prop_split_inv c xs 
    = let ys = split c xs in 
      collect (length ys) $ unsplit c ys == xs

main = quickCheck prop_split_inv
-- /show
