
import Test.QuickCheck
import ListWithIndex

propIndexIsInRange :: ListWithIndex a -> Property
propIndexIsInRange EmptyListWithNoIndex = classify True "length == 0" $ property True
propIndexIsInRange (ListWithIndex list index)
  = classify (length list > 3) "length > 3" $
    classify (length list == 2) "length == 2" $
    classify (length list == 1) "length == 1" $
    classify (length list == 0) "length == 0" $
    if null list
        then index == 0
        else index >= 0 && index < length list

propShrunkIndexIsInRange :: Arbitrary a => ListWithIndex a -> Property
propShrunkIndexIsInRange lwi
  = {- classify True shrinkee $ -} conjoin $ map propIndexIsInRange $ shrink lwi
        where shrinkee = show (length (shrink lwi)) ++ " long and " ++ show (sum (map len (shrink lwi)))
              len (ListWithIndex list _) = length list
              len EmptyListWithNoIndex = 0

propXYZ :: Property
propXYZ = conjoin [p1, p2, p3]
    where p1 = classify (4 > 3) "good programming language" $ (4 > 3)
          p2 = classify (4 > 3) "great programming language" $ (4 > 3)
          p3 = classify (4 > 3) "superb programming language" $ (4 > 3)

main = quickCheckWith stdArgs { maxSuccess = 500 } (propShrunkIndexIsInRange :: ListWithIndex Int -> Property)
--main = quickCheck propXYZ
