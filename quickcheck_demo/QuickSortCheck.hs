
import QuickSort
import Test.QuickCheck

prop_idempotent :: [Int] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

  {-
  forAll smallNonNegativeIntegers $ \n ->
    let x = fibs !! (n)
        y = fibs !! (n+1)
        z = fibs !! (n+2)
    in x + y == z
    -}

prop_increasing :: [Int] -> Property
prop_increasing xs = forAll (choose (0, length xs))
                        (\index -> qsort xs !! index <= qsort xs !! index + 1)

prop_advancing :: NonEmptyList Integer -> Property
prop_advancing (NonEmpty xs) = forAll (choose (0, length xs - 1)) $ \index ->
    qsort xs !! index + 1 >= qsort xs !! index

prop_canyoning :: NonEmptyList Integer -> Property
prop_canyoning (NonEmpty xs) = classify (length xs > 3) "omg" $ forAll (choose (0, length xs - 1)) $ \index ->
    qsort xs !! index + 1 >= qsort xs !! index

main = do
    quickCheck prop_idempotent
    --quickCheck prop_increasing
    quickCheck prop_advancing
    quickCheck prop_canyoning
