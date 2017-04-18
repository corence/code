
import Intents
import Actor
import TheSims
import qualified Data.Map as Map
import Data.Map(Map(..))
import Debug.Trace
import Data.List

--printGood _ = return ()
printGood = putStrLn . (++ "\n")

printBad = putStrLn . (++ "\n")

--printProgress = putStrLn
printProgress _ = return ()

--ctrace = trace
ctrace _ = id

run_preparation :: World -> [Intent World] -> String
run_preparation world initial = devolves (head (advance_intents world initial))

assert_preparation :: ([Intent World], [Intent World], World) -> IO ()
assert_preparation (initial, expected, world) = do
    printProgress $ "assert_preparation: " ++ devolves initial ++ " -> " ++ devolves expected
    assert_equal (run_preparation world initial) (devolves expected)

assert_preparations :: [([Intent World], [Intent World], World)] -> IO ()
assert_preparations tests = sequence_ $ map assert_preparation tests

--assert_someday_match :: World -> Int -> [Intent World] -> World -> IO ()
--assert_someday_match expected_world = assert_someday (== expected_world)

-- given intents and an initial world, assert that eventually the world will match the given predicate
assert_someday :: String -> (World -> Bool) -> Int -> [Intent World] -> World -> IO ()
assert_someday name predicate iterations intents world
  | predicate world = printGood $ "√ succeeds: \"" ++ name ++ "\""
  | iterations == 0 = printBad $ "† exhausted: \"" ++ name ++ "\"\nfinal intents " ++ show (map devolve intents) ++ ", final world " ++ dump_world world
  | intents_ready intents = ctrace ("ready intents: " ++ show intents ++ " -> " ++ show new_intents ++ "\n" ++ "new world: " ++ dump_world new_world) $ assert_someday name predicate (iterations - 1) new_intents new_world
  | otherwise = ctrace ("prepping intents: " ++ show intents) $ assert_someday name predicate (iterations - 1) prepared_intents world
  where prepared_intents = head (advance_intents world intents)
        new_world = foldr (\action world -> action world) world actions 
        (new_intents, actions) = intents_extract_actions intents

assert_equal :: (Show a, Eq a) => a -> a -> IO ()
assert_equal x y = if x == y
                       then printGood $ "√ " ++ show x ++ " is good"
                       else printBad $ "† " ++ show x ++ " /= " ++ show y

-- convert an Intent into a simple String -- so it can be compared with others for testing
devolve :: Intent World -> String
devolve (HazyIntent goal) = goal_name goal
devolve (OptionyIntent goal tasks) = goal_name goal ++ " [" ++ concat (intersperse ", " (map task_name tasks)) ++ "]"
devolve (TaskIntent goal task) = goal_name goal ++ " " ++ task_name task
devolve (ExecutableIntent goal task) = goal_name goal ++ " " ++ task_name task ++ "!"

devolves :: [Intent World] -> String
devolves intents = show $ map devolve intents

base_goal = be_unhungry 1
base_task = eat 1

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

prep_tests :: [([Intent World], [Intent World], World)]
prep_tests = [
            (
                [HazyIntent (be_unhungry 1)], -- from a hazy intent,
                [OptionyIntent (be_unhungry 1) [eat 1]], -- produce options!
                hungry_world
            ),
            (
                [OptionyIntent (be_unhungry 1) [eat 1]], -- from options,
                [TaskIntent (be_unhungry 1) (eat 1)], -- produce clarity!
                hungry_world
            ),
            (
                [TaskIntent (be_unhungry 1) (eat 1)], -- from the intent,
                [HazyIntent (have_food 1 1), TaskIntent (be_unhungry 1) (eat 1)], -- raise the prerequisite
                hungry_world
            ),
            (
                [HazyIntent (have_food 1 1), TaskIntent (be_unhungry 1) (eat 1)], -- with an intent to have food,
                [OptionyIntent (have_food 1 1) [], TaskIntent (be_unhungry 1) (eat 1)], -- no options will arise -- there's no food in this world
                hungry_world
            ),
            {- this is an error case now. Someday it can be reimplemented based on Desires (
                [OptionyIntent (have_food 1 1) [], TaskIntent (be_unhungry 1) (eat 1)], -- with no options on the table,
                [], -- our hero will swiftly give up
                hungry_world
            ), -}
            (
                [HazyIntent (have_food 1 1), TaskIntent (be_unhungry 1) (eat 1)], -- with an intent to have food,
                [OptionyIntent (have_food 1 1) [
                    take_item "food" 4 1,
                    take_item "food" 5 1,
                    cook 2 1,
                    cook 3 1
                    ],
                    TaskIntent (be_unhungry 1) (eat 1)], -- options will arise -- there's lots of food to choose from in this world
                bountiful_world
            ),
            (
                [OptionyIntent (have_food 1 1) [
                    take_item "food" 4 1,
                    take_item "food" 5 1,
                    cook 2 1,
                    cook 3 1
                    ],
                    TaskIntent (be_unhungry 1) (eat 1)], -- with many edible options,
                [TaskIntent (have_food 1 1) (take_item "food" 4 1), TaskIntent (be_unhungry 1) (eat 1)], -- we're gonna pick the closest one
                bountiful_world
            ),
            (
                [TaskIntent (have_food 1 1) (take_item "food" 4 1), TaskIntent (be_unhungry 1) (eat 1)], -- i'm so going for food
                [HazyIntent (be_at 4 1), TaskIntent (have_food 1 1) (take_item "food" 4 1), TaskIntent (be_unhungry 1) (eat 1)], -- so i'm gonna be there!
                bountiful_world
            )
        ]

someday_tests :: IO ()
someday_tests = sequence_ [
        assert_someday "the actor will feed herself somehow" (\world -> query_actor 1 unhungry world) 280 [HazyIntent (be_unhungry 1)] bountiful_world,
        assert_someday "item 4 will have no food at some point, and the actor will have food" (\world -> query_actor 1 ((== 1) . (get_item "food")) world && query_actor 4 ((== 0) . (get_item "food")) world) 380 [HazyIntent (be_unhungry 1)] bountiful_world,
        assert_someday "item 4 will be down to 1 food at some point, and the actor will have food" (\world -> query_actor 1 ((== 1) . (get_item "food")) world && query_actor 4 ((== 1) . (get_item "food")) world) 380 [HazyIntent (be_unhungry 1)] bountiful_world,
        assert_someday "the actor will cook" (\world -> query_actor 1 unhungry world) 280 [HazyIntent (be_unhungry 1)] cookable_world
        ]

main :: IO ()
main = do
    assert_equal 4 (length (seek_item "food" 1 [1] bountiful_world))
    assert_preparations prep_tests
    someday_tests
