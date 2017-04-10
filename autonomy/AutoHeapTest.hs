
import qualified AutoHeap as AutoHeap
import AutoHeap(AutoHeap(..))

assertish :: (Show a, Eq a) => Maybe a -> a -> IO ()
assertish Nothing b = putStrLn $ "a was Nothing; b was " ++ show b ++ "\n"
assertish (Just a) b = assert a b

assert :: (Show a, Eq a) => a -> a -> IO ()
assert a b
  | a == b = putStrLn $ "OK: " ++ show a ++ "\n"
  | otherwise = putStrLn $ show a ++ " != " ++ show b ++ "\n"

operations = [
                 (AutoHeap.add (3, "three"), 1, (3, "three")),
                 (AutoHeap.add (9, "nine"), 2, (3, "three")),
                 (AutoHeap.add (7, "seven"), 3, (3, "three")),
                 (AutoHeap.add (7, "seven"), 4, (3, "three")),
                 (AutoHeap.add (1, "one"), 5, (1, "one")),
                 (AutoHeap.remove_head, 4, (3, "three")),
                 (AutoHeap.remove_head, 3, (7, "seven")),
                 (AutoHeap.remove_head, 2, (7, "seven")),
                 (AutoHeap.remove_head, 1, (9, "nine"))
             ]

run_operation :: (Show a, Eq a) => ((AutoHeap a -> AutoHeap a), Int, a) -> AutoHeap a -> IO ()
run_operation (func, count, value) aheap = do
    let aheap2 = func aheap
    (assert (AutoHeap.size aheap2) count)
    assertish (AutoHeap.query aheap2) value

main = return ()


--main = 
    --let heap0 = AutoHeap compare AutoHeap.void 
    --assert (AutoHeap.size heap0) 0
    
    --let heap1 = AutoHeap.add (3, "three") heap0
    --assertish (AutoHeap.query heap1) (3, "three")
    --let heap2 = AutoHeap.add (9, "nine") heap1
    --assertish (AutoHeap.query heap2) (3, "three")
    --let heap3 = AutoHeap.add (7, "seven") heap2
    --assertish (AutoHeap.query heap3) (3, "three")
    --let heap4 = AutoHeap.add (1, "one") heap3
    --assertish (AutoHeap.query heap4) (1, "one")
    --assert (AutoHeap.size heap4) 4
    --let heap5 = AutoHeap.remove_head heap4
    --assert (AutoHeap.size heap4) 3
    --assertish (AutoHeap.query heap5) (3, "three")
    --let heap6 = AutoHeap.remove_head heap5
    --assertish (AutoHeap.query heap6) (7, "seven")
    --let heap7 = AutoHeap.remove_head heap6
    --assertish (AutoHeap.query heap7) (9, "nine")
    --assert (AutoHeap.size heap7) 1
    --let heap8 = AutoHeap.remove_head heap7
    --assert (AutoHeap.query heap8) Nothing
    --assert (AutoHeap.size heap8) 0
    --let heap9 = AutoHeap.remove_head heap8 -- this will now do an error
    --assert (AutoHeap.query heap9) Nothing
    --assert (AutoHeap.size heap9) 0
