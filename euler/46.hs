{-
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
-}

import Control.Monad

primes :: Integral a => [a]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

squares :: Integral a => [a]
squares = map square [1..]
    where square x = x * x

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n `mod` 2 == 0 = False
  | otherwise = all (\q -> not $ n `mod` q == 0) [2..n-1]

isComposite :: Int -> Bool
isComposite = not . isPrime

oddComposites = [x | x <- [3, 5..], isComposite x]

isGoldbachNum :: Int -> Bool
isGoldbachNum num = not $ null $ do
    prime <- takeWhile (< num) primes
    square <- takeWhile (< num) squares
    guard (num == prime + 2 * square)

main = putStrLn $ show $ head $ filter (not . isGoldbachNum) oddComposites
