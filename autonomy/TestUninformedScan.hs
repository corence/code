
import UninformedScan4
import Debug.Trace
import TheSims
import Intents

import qualified Actor as Actor
import Actor(ActorID, Actor(..), ItemID, Pos)

import qualified Data.Map as Map
import Data.Map(Map(..))

import qualified AutoHeap as AutoHeap
import AutoHeap(AutoHeap(..))

--printGood _ = return ()
printGood = putStrLn . (++ "\n")

printBad = putStrLn . (++ "\n")

--printProgress = putStrLn
printProgress _ = return ()

ctrace = trace
--ctrace _ = id

assert_equal :: (Show a, Eq a) => a -> a -> IO ()
assert_equal x y = if x == y
                       then printGood $ "√ " ++ show x ++ " is good"
                       else printBad $ "† " ++ show x ++ " /= " ++ show y

hungry_state = Map.fromList [(1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52)]))]
bountiful_state = Map.fromList [
    (1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52), ("prepped_ingredients", 52)])),
    (2, Actor 2 18 (Map.fromList [("oven", 1)])),
    (3, Actor 3 0 (Map.fromList [("oven", 1)])),
    (4, Actor 4 44 (Map.fromList [("food", 2)])),
    (5, Actor 5 99 (Map.fromList [("food", 100)]))
    ]
cookable_state = Map.fromList [
    (1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52)])),
    (2, Actor 2 18 (Map.fromList [("oven", 1)])),
    (3, Actor 3 0 (Map.fromList [("oven", 1)])),
    (4, Actor 4 44 (Map.fromList [("food", 0)])),
    (5, Actor 5 99 (Map.fromList [("food", 0), ("prepped_ingredients", 12)]))
    ]
    
doit :: Int -> AutoHeap (PartialResolution world) -> IO ()
doit 0 _ = return ()
doit n aheap = if AutoHeap.is_empty aheap
                  then return ()
                  else putStrLn (dump aheap) >> doit (n-1) (advance_resolutions aheap)

dump :: AutoHeap (PartialResolution world) -> String
dump aheap = "\n" ++ show (AutoHeap.size aheap) ++ "\n" ++ dump2 aheap ++ "\n"

dump2 :: AutoHeap (PartialResolution world) -> String
dump2 aheap = case first of
                Just pr -> dump3 pr ++ "\n" ++ dump2 others
                Nothing -> ""
    where first = AutoHeap.query aheap
          others = AutoHeap.remove_head aheap

dump3 :: (PartialResolution world) -> String
dump3 (PartialResolution priority (PartialSolution intent world cost stack))
  = "PartialResolution: "
  ++ " priority=" ++ show priority
  ++ " goal=" ++ goal_name (intent_goal intent)
  ++ " cost=" ++ show cost
  ++ " stacksize=" ++ show (length stack)

main :: IO ()
main = do
    doit 99 (start_resolving bountiful_state [Desire (be_unhungry 1) (const 44)])
    return ()
