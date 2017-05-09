-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Test.QuickCheck
import Control.Lens

isPalindromicNumber :: Int -> Bool
isPalindromicNumber number = isPalindrome (show number) ""

-- move chars from left to right, then check both lists for equality
isPalindrome :: String -> String -> Bool
isPalindrome left right
  | length left == length right = left == right
  | length ls == length right = isPalindrome ls right -- if there's one extra character in the middle, throw it away -- "cac" is a palindrome, and can ignore the 'a'
  | null left = error "impossible bug in isPalindrome"
  | otherwise = isPalindrome ls (l:right)
  where (l:ls) = left
        (r:rs) = right

numbers :: Int -> [Int]
numbers n = [n,n-1..0]

range :: Int -> Int -> [Int]
range maxi mini = [maxi,maxi-1..mini]

denouncePalindromic :: Int -> Int -> Int -> Int
denouncePalindromic maxi left right = max myResult nextResult
    where product = left * right
          myResult = if show product == reverse (show product) then product else 0
          nextResult = if left > maxi
            then if right > maxi
                    then 0
                    else denouncePalindromic maxi 0 (right + 1)
            else denouncePalindromic maxi (left + 1) right

obtusePalindromic :: Int
obtusePalindromic = maximum $ do
    a <- [100..999]
    b <- [a..999]
    let prod = a * b
    if reverse (show prod) == show prod
         then return prod
         else []

revenkPalindromic :: (Int, Int, Int)
revenkPalindromic = foldr tryIncrease (0, 0, 0) [100..999]

tryIncrease :: Int -> (Int, Int, Int) -> (Int, Int, Int)
tryIncrease left result = foldr (tryEnhance left) (0, 0, 0) [left..999]

tryEnhance :: Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
tryEnhance left right result = if reverse (show product) == show product && product > winner
                                             then (product, left, right)
                                             else result
                                             where product = left * right
                                                   (winner, _, _) = result

pshawPalindromic :: Int
pshawPalindromic = foldr tryIncrease 0 inputs
    where inputs = [(x * y) | x <- [100..999], y <- [x..999]]
          isPalindromicNumber n = reverse (show n) == show n
          tryIncrease candidate bestSoFar = if candidate > bestSoFar && isPalindromicNumber candidate
                                                then candidate
                                                else bestSoFar
                                                    

{-
max = 0
100.upto(999) { |a|
  a.upto(999) { |b|
    prod = a * b
    max = [max, prod].max if prod.to_s == prod.to_s.reverse
  }
}
puts "Maximum palindrome is #{ max }."
-}

producePalindromic :: Int -> Int -> (Int, Int, Int) -- result is (Result, num1, num2) where Result = num1 * num2
producePalindromic maxi fulcrum 
  = if fulcrum > 0
        then max this that
        else (0, 0, 0)
  where this = head $ filter (\(r, _, _) -> isPalindromicNumber r) $ map (\num -> (num * fulcrum, num, fulcrum)) (range maxi 0)
        that = producePalindromic maxi (fulcrum - 11)

-- find the biggest palindromic number that is a multiple of two numbers
-- The numbers can't be higher than maxValue, and the second number must be divisible by 11
-- If a result has been found, it'll be in the third parameter so we know to abort the search
summonPalindromic :: Int -> Int -> Maybe Int -> Int
summonPalindromic maxValue fulcrum maybeMaxResult
  = if fulcrum < 11
      then 0
      else max maxSoFar $ summonPalindromic maxValue (fulcrum - 11) (Just maxSoFar)
          where results = filter isPalindromicNumber $ map (\v -> v * fulcrum) [maxValue,maxValue-1..fulcrum]
                result = case results of
                              (x:xs) -> x
                              [] -> 0
                maxSoFar = case maybeMaxResult of
                            Just maxResult -> max maxResult result
                            Nothing -> result

seekPalindromics :: (Int, Int) -> Int
seekPalindromics (0, 0) = 0
seekPalindromics (number1, number2)
  = let number = number1 * number2 in
        if isPalindromicNumber number
            then number
            else if number1 <= number2
                    then right
                    else max left right
    where left = seekPalindromics (number1 - 1, number2)
          right = seekPalindromics (number1, number2 - 1)

findPalindromics :: (Int, Int) -> [Int]
findPalindromics (0, 0) = []
findPalindromics (number1, number2)
  = let number = number1 * number2 in
        if isPalindromicNumber number
            then number : (left ++ right)
            else left ++ right
    where left = if number1 <= number2 then [] else findPalindromics (number1 - 1, number2)
          right = findPalindromics (number1, number2 - 11)

--main = putStrLn $ show $ seekPalindromics (99, 99)
--main = putStrLn $ show $ summonPalindromic 99 99 Nothing
--main = putStrLn $ show $ producePalindromic 999 990
--main = putStrLn $ show $ denouncePalindromic 999 999 990
main = putStrLn $ show obtusePalindromic ++ ",,,," ++ show revenkPalindromic ++ "......." ++ show pshawPalindromic
{-
main = do
    quickCheckWith stdArgs { maxSuccess = 1500 } propPalindromesDivisibleBy11
    quickCheckWith stdArgs { maxSuccess = 1500 } propPalindromeFromString
    -}

propPalindromesDivisibleBy11 number 
    = classify suitable "palindromic" 
    $ suitable ==> (number `mod` 11 == 0)
    where suitable = (isPalindromicNumber number && number >= 10)

propPalindromeFromString :: (Bool, String) -> Bool
propPalindromeFromString (even, base) = isPalindrome constructedString ""
    where constructedString = if even || null base
                                  then reverse base ++ base
                                  else reverse base ++ tail base

    
-- this property is false; 13 * 11 = 143
--propDiv11IsPalindromic :: Positive Int -> Bool
--propDiv11IsPalindromic number = isPalindromicNumber (getPositive number * 11)
    
