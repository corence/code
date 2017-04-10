
import qualified AutoHeap as AutoHeap
import AutoHeap(AutoHeap(..))

assertish :: (Show a, Eq a) => Maybe a -> a -> IO ()
assertish Nothing b = putStrLn $ "a was Nothing; b was " ++ show b ++ "\n"
assertish (Just a) b = assert a b

assert :: (Show a, Eq a) => a -> a -> IO ()
assert a b
  | a == b = putStrLn $ "OK: " ++ show a ++ "\n"
  | otherwise = putStrLn $ show a ++ " != " ++ show b ++ "\n"

main = do
    let heap0 = AutoHeap compare AutoHeap.void 
    let heap1 = AutoHeap.add (3, "three") heap0
    assertish (AutoHeap.query heap1) (3, "three")
    let heap2 = AutoHeap.add (9, "nine") heap1
    assertish (AutoHeap.query heap2) (3, "three")
    let heap3 = AutoHeap.add (7, "seven") heap2
    assertish (AutoHeap.query heap3) (3, "three")
    let heap4 = AutoHeap.add (1, "one") heap3
    assertish (AutoHeap.query heap4) (1, "one")
    assert (AutoHeap.size heap4) 4
    let heap5 = AutoHeap.remove_head heap4
    assertish (AutoHeap.query heap5) (3, "three")
    let heap6 = AutoHeap.remove_head heap5
    assertish (AutoHeap.query heap6) (7, "seven")
    let heap7 = AutoHeap.remove_head heap6
    assertish (AutoHeap.query heap7) (9, "nine")
    assert (AutoHeap.size heap7) 1
    let heap8 = AutoHeap.remove_head heap7
    assert (AutoHeap.query heap8) Nothing
    assert (AutoHeap.size heap8) 0
    --let heap9 = AutoHeap.remove_head heap8 -- this will now do an error
    --assert (AutoHeap.query heap9) Nothing
    --assert (AutoHeap.size heap9) 0
