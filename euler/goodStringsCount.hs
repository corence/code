{-
Given an integer len, count the number of different good strings that have a length of exactly len. A good string is a string for which the following conditions are true:

A good string contains only lowercase English letters.
Each character in a good string is unique.
Exactly one character in a good string is lexicographically greater than the character that precedes it.
-}

import Data.Char
import Data.List
import Control.Monad

chars = ['a'..'z']

allPossibleStrings :: Int -> [String]
allPossibleStrings 0 = []
allPossibleStrings len = do
    char <- chars
    string <- allPossibleStrings (len - 1)
    return $ char : string

-- WRONG BITCH
-- "ac" is lexi greater too
isGoodString :: String -> Bool
isGoodString string = hasNoDuplicates string && hasAscenders 1 1 string
    where hasNoDuplicates string = hasAscenders 0 0 $ sort string
          hasAscenders _ 0 [] = True
          hasAscenders _ 0 (x:[]) = True
          hasAscenders _ _ [] = False
          hasAscenders _ _ (x:[]) = False
          hasAscenders step num (c:d:cs) = if ord d - ord c == step
                                            then hasAscenders step (num - 1) (d : cs)
                                            else hasAscenders step num (d : cs)

goodStringsCount 1 = 0
goodStringsCount 2 = 325
--goodStringsCount len = (len - 1) * (descend 24 (len - 2)) * goodStringsCount 2
goodStringsCount len = length $ filter isGoodString $ allPossibleStrings len

-- we know that goodStringsCount 2 is 325.
-- Beyond that, we're adding arbitrary characters that aren't involved in condition 3.
-- Thus, we can consider that:
--  * there's only 24 characters left in the alphabet to choose from
--  * they still need to be unique -- hence the descend function
--  * after we choose our arbitrary set of characters, we then need to select where to insert the pair of increasing characters
--  * oh fuck -- _exactly one_ is greater. that's not accounted for.

-- examples, 4:
-- ab c d # c has 1 bad value...unless b is "z". d has 1 bad value unless c is "z".
-- c ab d # c has 1 bad value -- unless a is "a". d has 1 bad value (unless)
-- ab c d # same shit again
-- 
-- examples, 5:
-- ab c d e # same again.

-- count 3 should be:
-- c has 26 values
-- a has 24 values
-- b has a whole ton of different possible values.
-- 

-- maybe we generate the bad cases, then filter them out?
-- ab c -> generate ab, then generate c, then eliminate c > b -- there will be 
descend numer 0 = 1
descend numer n = numer * descend (numer - 1) (n - 1)

main = return ()

-- consider if the alphabet is just abcde
-- 2: ab ac ad ae bc bd be cd ce de -> 10
-- 3: (with the pair last) bac bad bae cab cad cae cbd cbe dab dac dae dbc dbe dce eab eac ead ebc ebd ecd -> 20
--    (with the pair first) acb adb adc aeb aec aed bca bda bdc bea bec bed cda cdb cea ceb ced dea deb dec -> 20
-- 4: (pair first) adcb aecb aedb aedc bdca beca beda bedc cdba ceba ceda cedb deba deca decb -> 15
--    (pair second) badc baec baed cadb caeb caed cbda cbea cbed dacb daeb daec dbca dbea dbec dcea dceb eacb eadb eadc ebca ebda ebdc ecda ecdb -> 25
--    (pair third) cbad cbae dbac dbae dcab dcae dcbe ebac ebad ecab ecad ecbd edab edac edbc -> 15
-- 4: cbad cdba cbda dbca bdca dbac badc dacb adcb dcab cadb decb dceb dcbe ecdb cedb ecbd cbed ebdc bedc edbc dbec ebac baec ebca beca eacb aecb ecab caeb ceba cbea cbae eadc aedc edac daec deca dcea dcae ecad caed ecda ceda eadb aedb edab daeb deba dbea dbae ebad baed ebda beda -> 55

-- chars in alphabet, length
allPossibleStrings2 :: [Char] -> Int -> [String]
allPossibleStrings2 chars len = if (len > length chars) then [] else nub $ map (take len) $ permutations chars

allGoodStrings chars len = filter isGoodString2 $ allPossibleStrings2 chars len

isGoodString2 :: String -> Bool
isGoodString2 string = numAscenders string == 1
    where numAscenders [] = 0
          numAscenders (x:[]) = 0
          numAscenders (x:y:xs)
            | y > x = 1 + numAscenders (y:xs)
            | otherwise = numAscenders (y:xs)
