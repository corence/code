
module ThreadingRandom where

import System.Random

-- random :: (RandomGen g, Random a) => g -> (a, g)

-- generates 3 random numbers, adds them together, and returns the result (along with the updated random generator)
-- it's called annoying because it's fucking annoying to write in this style
annoying :: RandomGen g => g -> (Int, g)
annoying gen0 = (num1 + num2 + num3, gen3)
    where (num1, gen1) = random gen0
          (num2, gen2) = random gen1
          (num3, gen3) = random gen2
          

-- generates 3 random numbers, adds them together, and returns the result. Less good because it doesn't return the random generator
nicer1 :: RandomGen g => g -> Int
nicer1 gen0 = sum (take 3 (randoms gen0))

-- same as nicer1 but in a briefer style
nicer2 :: RandomGen g => g -> Int
nicer2 = sum . take 3 . randoms

-- this is the same behaviour as "annoying". Cool!
-- ~> operator is defined below
threaded :: RandomGen g => g -> (Int, g)
threaded gen0 = random gen0 ~> (+) ~> (+)

-- given a value and a random generator:
--  - generates a random number using the generator
--  - applies the old and new values to a 2-argument function
--  - returns the result of that function, and the updated generator
(~>) :: (RandomGen g, Random b) => (a, g) -> (a -> b -> c) -> (c, g)
(~>) (a, gen0) func =
    let (b, gen1) = random gen0
    in (func a b, gen1)

-- still trying to make a better solution... let's write a better "randoms" so we can make the list of random numbers upfront
-- but we still need helpers for unwrapping tuples, otherwise it's too annoying
-- i followed elm restrictions in this method -- no multiple pattern matchers in the function, and no `where`... so it's uglier than it could have been imo
soRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
soRandoms num gen0 =
    if num > 0
        then let (value, gen1) = random gen0
                 (values, gen2) = soRandoms (num - 1) gen1
             in (value : values, gen2)
        else ([], gen0)

-- takes the first element of a tuple, passes it into a function, then returns the result wrapped in a new tuple
useTupleFirst :: (a, b) -> (a -> c) -> (c, b)
useTupleFirst (a, b) func = (func a, b)

-- operator alias for useTupleFirst
-- i tried making a sensible operator about tuples without using commas or parens (as those are disallowed) -- it's hard
-- so then i tried a table flip, but these were the only characters that haskell accepted
(┻━┻) :: (a, b) -> (a -> c) -> (c, b)
(┻━┻) = useTupleFirst

-- this produces the same output as "annoying" and "threaded"
add3Randoms :: RandomGen g => g -> (Int, g)
add3Randoms gen0 = soRandoms 3 gen0 ┻━┻ sum
-- if you hate operators, here it is without operators:

add3Randoms2 gen0 = useTupleFirst (soRandoms 3 gen0) sum

-- now let's see how this is useful in more general contexts
data Triangle = Triangle Int Int Int deriving Show

-- makes a triangle, with 3 random edge lengths. Totally works.
makeRandomTriangle :: RandomGen g => g -> (Triangle, g)
makeRandomTriangle gen0 = soRandoms 3 gen0 ┻━┻ (\[a, b, c] -> Triangle a b c) -- i'm unsure if this is moral. if soRandoms returns a list that isn't of length 3, we'll crash. Probably shouldn't do it like this.


{-
-- i still don't like these solutions -- i think we'd be better off if we can generate random tuples...
-- but we can't do that because we'd be restricting the generality of the Random typeclass to do so!
-- so this doesn't work.
instance (Random a) => Random (a, a, a) where
    random gen0 = let (a, gen1) = random gen0
                      (b, gen2) = random gen1
                      (c, gen3) = random gen2
                  in ((a, b, c), gen3)
    randomR range gen0 = let (a, gen1) = randomR range gen0
                             (b, gen2) = randomR range gen1
                             (c, gen3) = randomR range gen2
                         in ((a, b, c), gen3)
-}


-- so let's go back to generating random lists and see if we can do that better:

-- check this out -- this little rocket is gonna thread our generator and we'll end up with a list of random values
(~~>) :: (RandomGen g, Random a) => ([a], g) -> (g -> (a, g)) -> ([a], g)
(~~>) (values, gen0) func =
    let (value, gen1) = func gen0
    in (value : values, gen1)
    
-- and this one will allow us to complete the chain
-- actually this is just the "first thing in a tuple" operator again
(~=>) :: (RandomGen g, Random a) => ([a], g) -> ([a] -> b) -> (b, g)
(~=>) (values, gen0) func = (func values, gen0)

-- and this one will get our sequence started
wrapGen :: (RandomGen g, Random a) => g -> ([a], g)
wrapGen gen0 = ([], gen0)
    
-- i wanted to use this operator instead of wrapGen but i don't really know how unary operators work
(~~) :: (RandomGen g, Random a) => g -> ([a], g)
(~~) = wrapGen
    
makeRandomTriangle2 :: RandomGen g => g -> (Triangle, g)
makeRandomTriangle2 gen0 = (wrapGen gen0) ~~> random ~~> random ~~> random ~=> (\[a, b, c] -> Triangle a b c)
