
import qualified AutoHeap as AutoHeap
import AutoHeap(AutoHeap(..))

assertish :: (Show a, Eq a) => Maybe a -> a -> IO ()
assertish Nothing b = putStrLn $ "a was Nothing; b was " ++ show b ++ "\n"
assertish (Just a) b = assert a b

assert :: (Show a, Eq a) => a -> a -> IO ()
assert a b
  | a == b = putStrLn $ "OK: " ++ show a ++ "\n"
  | otherwise = putStrLn $ show a ++ " != " ++ show b ++ "\n"

action :: (Show a, Eq a) => (AutoHeap a -> AutoHeap a) -> a -> Int -> AutoHeap a -> IO (AutoHeap a)
action func expected_value expected_size aheap = do
    let new_aheap = func aheap
    assertish (AutoHeap.query new_aheap) expected_value
    assert (AutoHeap.size new_aheap) expected_size
    return new_aheap
    
action_nothing :: (Show a, Eq a) => (AutoHeap a -> AutoHeap a) -> AutoHeap a -> IO (AutoHeap a)
action_nothing func aheap = do
    let new_aheap = func aheap
    assert (AutoHeap.query new_aheap) Nothing
    assert (AutoHeap.size new_aheap) 0
    return new_aheap

main = do
    action_nothing id (AutoHeap.void compare)
        >>= action (AutoHeap.add (9, "nine")) (9, "nine") 1
        >>= action (AutoHeap.add (3, "three")) (3, "three") 2
        >>= action (AutoHeap.add (7, "seven")) (3, "three") 3
        >>= action (AutoHeap.add (1, "one")) (1, "one") 4
        >>= action (AutoHeap.remove_head) (3, "three") 3
        >>= action (AutoHeap.add (7, "seven")) (3, "three") 4
        >>= action (AutoHeap.remove_head) (7, "seven") 3
        >>= action (AutoHeap.remove_head) (7, "seven") 2
        >>= action (AutoHeap.remove_head) (9, "nine") 1
        >>= action_nothing (AutoHeap.remove_head)
        >>= action (AutoHeap.add (3, "three")) (3, "three") 1
        >>= action (AutoHeap.add (9, "nine")) (3, "three") 2
