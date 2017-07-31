
{-
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
 -}
 
import Data.List

sample :: [Int]
sample = [1,4,0,6,3,5,7,2,8,9]

divisible :: Int -> Int -> Int -> [Int] -> Bool
divisible index length divisor array = numerate sublist `mod` divisor == 0
    where sublist = take length (drop index array)

special :: [Int] -> Bool
special array =
    divisible 1 3 2 array &&
    divisible 2 3 3 array &&
    divisible 3 3 5 array &&
    divisible 4 3 7 array &&
    divisible 5 3 11 array &&
    divisible 6 3 13 array &&
    divisible 7 3 17 array
    
numerate :: [Int] -> Int
numerate [] = 0
numerate (digit:digits) = fromIntegral digit * (10 ^ fromIntegral (length digits)) + numerate digits

main = putStrLn $ show $ result
    --where result = sum (filter special (permutations [0,1,2,3,4,5,6,7,8,9]))
    where allPossibleSequences = permutations [0,1,2,3,4,5,6,7,8,9]
          validSequences = filter special allPossibleSequences
          result = sum (map numerate validSequences)
