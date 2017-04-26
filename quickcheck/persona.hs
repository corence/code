import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Maybe

-- https://www.stuartgunter.org/posts/intro-to-quickcheck/

data Age     = Age Int
             deriving (Show, Eq)
unborn = Age 0

instance Arbitrary Age where
  arbitrary  = Age `liftM` choose (0, 100)


  
type Name    = String
data Person  = Person Name Age
             deriving (Show)

instance Arbitrary Person where
  arbitrary  = liftM2 Person arbitrary arbitrary
  shrink (Person name age) = catMaybes [maybeBlankName, maybeBlankAge, maybeBlankBoth]
    where maybeBlankName = if null name then Nothing else Just $ Person "" age
          maybeBlankAge  = if age == unborn then Nothing else Just $ Person name unborn
          maybeBlankBoth = if (age == unborn && null name) then Nothing else Just $ Person "" unborn
  
prop_PersonAge (Person name (Age ageNum))
  = ageNum <= 100
  
main = quickCheck prop_PersonAge



prop_Square :: Float -> Bool
prop_Square x =
  x^2 >= x

prop_CubeOfPositive :: NonNegative Integer -> Bool
prop_CubeOfPositive (NonNegative x) =
  x^3 >= x  

--main = quickCheck prop_CubeOfPositive

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
--main = putStrLn $ concat $ map show $ take 10 fibs

smallNonNegativeIntegers = choose (0, 500)

prop_Fibonacci =
  forAll smallNonNegativeIntegers $ \n ->
    let x = fibs !! (n)
        y = fibs !! (n+1)
        z = fibs !! (n+2)
    in x + y == z
--main = quickCheck prop_Fibonacci


prop_IdempotentSort :: [Float] -> Property
prop_IdempotentSort xs =
  classify (length xs < 2) "trivial" $
      sort (sort xs) == sort xs

--main = quickCheck prop_IdempotentSort >> quickCheck prop_Fibonacci
