module RefactoringPuzzle where

import Control.Monad.Reader

newtype IntRdr a =
  IntRdr {
    readIntRdr :: Int -> a
  }

instance Functor IntRdr where
    fmap f (IntRdr g) = IntRdr (f . g)

--instance Applicative IntRdr where
    --(<*>) f (IntRdr)

mapIntRdr ::
  IntRdr a
  -> (a -> b)
  -> IntRdr b
mapIntRdr (IntRdr g) f =
  IntRdr (f . g)

bindIntRdr ::
  IntRdr a
  -> (a -> IntRdr b)
  -> IntRdr b
bindIntRdr (IntRdr g) f =
  IntRdr (\n -> readIntRdr (f (g n)) n)

applyIntRdr ::
  a
  -> IntRdr a
applyIntRdr =
  IntRdr . const

type Option = Maybe

mapOption ::
  Option a
  -> (a -> b)
  -> Option b
mapOption Nothing _ =
  Nothing
mapOption (Just a) f =
  Just (f a)

bindOption ::
  Option a
  -> (a -> Option b)
  -> Option b
bindOption Nothing _ =
  Nothing
bindOption (Just a) f =
  f a

applyOption ::
  a
  -> Option a
applyOption a =
  Just a

-- Return all the Some values, or None if not all are Some.
runOptions :: [Option a] -> Option [a]
--runOptions = foldr (\a b -> a >>= (\aa -> fmap (aa:) b)) (pure [])
runOptions = sequence

-- Apply an Int to a list of int readers and return the list of return values.
runIntRdrs :: [IntRdr a] -> IntRdr [a]
runIntRdrs = foldr (\a b -> bindIntRdr a (\aa -> mapIntRdr b (aa:))) (applyIntRdr [])
--runIntRdrs = sequence

runIntRdr2 :: [Reader Int] -> Reader [Int]
runIntRdr2 = sequence

-- Code Duplication

-- ***      ******      *******      ****
-- runOptions :: [Option a] -> Option [a]
-- runIntRdrs :: [IntRdr a] -> IntRdr [a]

-- ***      ***********************      **************      ************           ****
-- runOptions = foldr (\a b -> bindOption a (\aa -> mapOption b (aa:))) (applyOption [])
-- runIntRdrs = foldr (\a b -> bindIntRdr a (\aa -> mapIntRdr b (aa:))) (applyIntRdr [])
