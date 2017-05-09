
module ListWithIndex
( ListWithIndex(..)
) where

import Test.QuickCheck

data ListWithIndex a = EmptyListWithNoIndex | ListWithIndex [a] Int deriving (Show, Eq)

instance Arbitrary a => Arbitrary (ListWithIndex a) where
{-
    arbitrary = do
            list <- arbitrary
            index <- choose (0, max 0 (length list - 1))
            return (ListWithIndex list index)
            -}
            
    arbitrary = do
            list <- arbitrary
            if null list
                then return EmptyListWithNoIndex
                else do
                    index <- choose (0, length list - 1)
                    return $ ListWithIndex list index
    shrink (ListWithIndex list index) = reindex (ListWithIndex list index) <$> shrink list
    shrink EmptyListWithNoIndex = [EmptyListWithNoIndex]

-- given an old list with an index, and a new list, generate a new index (and thus a new ListWithIndex)
reindex :: ListWithIndex a -> [a] -> ListWithIndex a
reindex EmptyListWithNoIndex _ = EmptyListWithNoIndex
reindex (ListWithIndex list index) newList
  = if null newList
      then EmptyListWithNoIndex
      else ListWithIndex newList newIndex
          where lengthDifference = length newList - length list
                newIndex = max 0 (index + lengthDifference)

{-
data ListWithIdx a = ListWithIdx [a] Int deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ListWithIdx a) where

  arbitrary = sized $ \n -> do
       k <- choose (0, n) -- list length
       -- added index
       i <- if k == 0 then return 0 else choose (0, k-1)
       
       list <- sequence [ arbitrary | _ <- [1..k] ]
       -- wrap them in the new type
       return $ ListWithIdx list i    

  shrink (ListWithIdx xs i) = 
     map wrap $ shrinkList shrink xs
      where
        lenXs = length xs
        wrap ys = assert (lenYs <= lenXs) $ ListWithIdx ys $ max 0 $ i - (lenXs - lenYs)
          where
            lenYs = length ys       
            -}
