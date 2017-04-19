
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

hungry_world = Map.fromList [(1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52)]))]
bountiful_world = Map.fromList [
    (1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52), ("prepped_ingredients", 52)])),
    (2, Actor 2 18 (Map.fromList [("oven", 1)])),
    (3, Actor 3 0 (Map.fromList [("oven", 1)])),
    (4, Actor 4 44 (Map.fromList [("food", 2)])),
    (5, Actor 5 99 (Map.fromList [("food", 100)]))
    ]
cookable_world = Map.fromList [
    (1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52)])),
    (2, Actor 2 18 (Map.fromList [("oven", 1)])),
    (3, Actor 3 0 (Map.fromList [("oven", 1)])),
    (4, Actor 4 44 (Map.fromList [("food", 0)])),
    (5, Actor 5 99 (Map.fromList [("food", 0), ("prepped_ingredients", 12)]))
    ]
    
doit :: Int -> AutoHeap (PartialResolution World) -> IO ()
doit 0 _ = return ()
doit n aheap = if AutoHeap.is_empty aheap
                  then return ()
                  else putStrLn (dump_decisions aheap) >> doit (n-1) (advance_resolutions aheap)

doit2 :: Int -> AutoHeap (PartialResolution World) -> IO ()
doit2 0 _ = return ()
doit2 n aheap = if AutoHeap.is_empty aheap
                  then return ()
                  else putStrLn (show (AutoHeap.size aheap)) >> doit2 (n-1) (advance_resolutions aheap)

dump_decisions :: AutoHeap (PartialResolution World) -> String
dump_decisions aheap = "\n" ++ show (AutoHeap.size aheap) ++ "\n" ++ dump_each_decision aheap ++ "\n"

dump_each_decision :: AutoHeap (PartialResolution World) -> String
dump_each_decision aheap = case first of
                Just pr -> "((" ++ show (AutoHeap.size aheap) ++ ")) " ++ dump_pr pr ++ "\n" ++ dump_each_decision others
                Nothing -> "((" ++ show (AutoHeap.size aheap) ++ ")) " ++ "no more"
    where first = AutoHeap.query aheap
          others = AutoHeap.remove_head aheap

dump_pr :: (PartialResolution World) -> String
dump_pr (PartialResolution priority (PartialSolution intent world cost stack))
  = "PartialResolution: "
  ++ " priority=" ++ show priority
  ++ " goal=" ++ goal_name (intent_goal intent)
  ++ dump_task (intent_task intent)
  ++ " cost=" ++ show cost
  ++ " stack: " ++ dump_stack stack
  -- ++ " stacksize=" ++ show (length stack)
  -- ++ if null stack then "" else " head: goal=" ++ goal_name (intent_goal (head stack))
  -- ++ if null stack then "" else show_task (intent_task (head stack))
  ++ dump_world world
            
dump_stack :: [Intent world] -> String
dump_stack [] = ""
dump_stack (intent : intents) = dump_stack intents ++ " -> " ++ goal_name (intent_goal intent) ++ perhaps_dump (\intent -> " => " ++ task_name intent) (intent_task intent) ++ " "

dump_task :: Maybe (Task world) -> String
dump_task Nothing = ""
dump_task (Just task) = " task=" ++ task_name task

perhaps_do :: b -> (a -> b) -> Maybe a -> b
perhaps_do default_string _ Nothing = default_string
perhaps_do _ func (Just a) = func a

perhaps_dump :: (a -> String) -> Maybe a -> String
perhaps_dump = perhaps_do ""

main :: IO ()
main = do
    doit 4 (start_resolving bountiful_world [Desire (be_unhungry 1) (const 44)])
    return ()
