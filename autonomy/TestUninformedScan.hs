
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

data Verbosity = NoVerbosity | StepCounter | FullInfo

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
    (1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 32), ("prepped_ingredients", 52)])),
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

-- expected from bountiful_world
-- 1) [be_unhungry eat]
-- 2) [have_item_food] (skipped)
-- 3) [take_item_food, take_item_food, cook, cook] (skipped)
-- 4) [goto_food, goto_food, goto_oven, goto_oven]
    
run_simulation :: Verbosity -> Int -> (Int, AutoHeap (PartialResolution World), Int) -> AutoHeap (PartialResolution World) -> IO ()
run_simulation _ 0 _ _ = return ()
run_simulation verbosity n statistics aheap = case AutoHeap.query aheap of
                 Nothing -> putStrLn "Aborted -- empty queue."
                 Just pr -> if is_victorious pr
                               then putStrLn ("\n\nSo victorious. Recommendation = " ++ dump_this_intent pr) >> print_outcomes statistics
                               else print_status >> run_simulation verbosity (n-1) (increment_statistics statistics) (advance_resolutions aheap)
                                   where is_victorious (PartialResolution _ (PartialSolution intent world _ _)) = goal_succeeds (intent_goal intent) world
                                         print_status = case verbosity of
                                                            NoVerbosity -> return ()
                                                            StepCounter -> putStrLn ("Step: " ++ show n) >> putStrLn (dump_decisions True aheap)
                                                            FullInfo -> putStrLn (show (AutoHeap.size aheap))
                                         print_outcomes (num_steps, broadest, total_breadths)
                                            = putStrLn ("Steps: " ++ show num_steps ++ ", biggest breadth: " ++ show (AutoHeap.size broadest) ++ ", average breadth: " ++ show (fromIntegral total_breadths / fromIntegral num_steps))
                                                >> putStrLn (dump_decisions False broadest)
                                         increment_statistics (num_steps, broadest, total_breadths) =
                                            (num_steps + 1,
                                            if AutoHeap.size aheap > AutoHeap.size broadest then aheap else broadest,
                                            total_breadths + (AutoHeap.size aheap))
                                         dump_this_intent (PartialResolution _ (PartialSolution intent _ _ _)) = dump_stack [intent]

dump_decisions :: Bool -> AutoHeap (PartialResolution World) -> String
dump_decisions should_dump_world aheap = dump_each_decision should_dump_world aheap ++ "\n"

dump_each_decision :: Bool -> AutoHeap (PartialResolution World) -> String
dump_each_decision should_dump_world aheap = case first of
                Just pr -> "((" ++ show (AutoHeap.size aheap) ++ ")) " ++ dump_pr should_dump_world pr ++ "\n" ++ dump_each_decision should_dump_world others
                Nothing -> "((" ++ show (AutoHeap.size aheap) ++ ")) " ++ "no more"
    where first = AutoHeap.query aheap
          others = AutoHeap.remove_head aheap

dump_pr :: Bool -> (PartialResolution World) -> String
dump_pr should_dump_world (PartialResolution priority (PartialSolution intent world cost stack))
  = "PartialResolution: "
  ++ " priority=" ++ show priority
  ++ " goal=" ++ goal_name (intent_goal intent)
  ++ dump_task (intent_task intent)
  ++ " cost=" ++ show cost
  ++ " stack: " ++ dump_stack stack
  -- ++ " stacksize=" ++ show (length stack)
  -- ++ if null stack then "" else " head: goal=" ++ goal_name (intent_goal (head stack))
  -- ++ if null stack then "" else show_task (intent_task (head stack))
  ++ if should_dump_world then dump_world world else ""
            
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
    run_simulation NoVerbosity 9999 (0, AutoHeap.void (const (const EQ)), 0) (start_resolving bountiful_world [Desire (be_unhungry 1) (const 44)])
    return ()
