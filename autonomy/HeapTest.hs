
import qualified Heap as Heap
import Heap(Heap(..))

assertish :: (Show a, Eq a) => Maybe a -> a -> IO ()
assertish Nothing b = putStrLn $ "a was Nothing; b was " ++ show b ++ "\n"
assertish (Just a) b = assert a b

assert :: (Show a, Eq a) => a -> a -> IO ()
assert a b
  | a == b = putStrLn $ "OK: " ++ show a ++ "\n"
  | otherwise = putStrLn $ show a ++ " != " ++ show b ++ "\n"

main = do
    let heap0 = Heap.void 
    let heap1 = Heap.add (3, "three") heap0
    assertish (Heap.query heap1) "three"
    let heap2 = Heap.add (9, "nine") heap1
    assertish (Heap.query heap2) "three"
    let heap3 = Heap.add (7, "seven") heap2
    assertish (Heap.query heap3) "three"
    let heap4 = Heap.add (1, "one") heap3
    assertish (Heap.query heap4) "one"
    assert (Heap.size heap4) 4
    let heap5 = Heap.remove_head heap4
    assertish (Heap.query heap5) "three"
    let heap6 = Heap.remove_head heap5
    assertish (Heap.query heap6) "seven"
    let heap7 = Heap.remove_head heap6
    assertish (Heap.query heap7) "nine"
    assert (Heap.size heap7) 1
    let heap8 = Heap.remove_head heap7
    assert (Heap.query heap8) Nothing
    assert (Heap.size heap8) 0
    --let heap9 = Heap.remove_head heap8 -- this will now do an error
    --assert (Heap.query heap9) Nothing
    --assert (Heap.size heap9) 0
