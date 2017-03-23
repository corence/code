
import Intents
import Actor
import IntentSims
import qualified Data.Map as Map
import Data.Map(Map(..))

run_preparation :: [Intent Command State] -> State -> [(String, [String])]
run_preparation initial state = map devolve new_intents
    where (changed, new_intents) = prepare_intents initial state

assert_preparation :: ([(String, [String])], [Intent Command State], State) -> IO ()
assert_preparation (expected, initial, state) = assert_equal expected (run_preparation initial state)

assert_preparations :: [([(String, [String])], [Intent Command State], State)] -> IO ()
--assert_preparations tests = fmap assert_preparation tests
{-
assert_preparations [] = return ()
assert_preparations (test : tests) = do
    assert_preparation test
    assert_preparations tests
    -}
--assert_preparations tests = map assert_preparation tests
assert_preparations tests = sequence_ $ map assert_preparation tests

devolve :: Intent Command State -> (String, [String])
devolve (Intent goal tasks) = (goal_name goal, tasks_string)
    where tasks_string = case tasks of
                             Just ts -> map task_name ts
                             Nothing -> []

base_goal = be_unhungry 1
base_task = eat 1

hungry_state = Map.fromList [(1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52)]))]
bountiful_state = Map.fromList [
    (1, Actor 1 14 (Map.fromList [("dude", 1), ("hunger", 52)])),
    (2, Actor 2 18 (Map.fromList [("oven", 1)])),
    (3, Actor 3 0 (Map.fromList [("oven", 1)])),
    (4, Actor 4 44 (Map.fromList [("food", 2)])),
    (5, Actor 5 99 (Map.fromList [("food", 100)]))
    ]

prep_tests :: [([(String, [String])], [Intent Command State], State)]
prep_tests = [
            ([("be_unhungry", ["eat"])], [Intent (be_unhungry 1) Nothing], hungry_state),
            ([("have_item", []), ("be_unhungry", ["eat"])], [Intent (be_unhungry 1) (Just $ [eat 1])], hungry_state),
            (
                [
                    ("have_item", []), -- this will come up blank because there's no food to pickup and no ovens to cook upon
                    ("be_unhungry", ["eat"])
                ], [
                    Intent (have_food 1 1) Nothing,
                    Intent (be_unhungry 1) (Just [eat 1])
                ], hungry_state
            ),
            (
                [
                    ("have_item", ["take_item 'food' from 4", "take_item 'food' from 5", "cook in this oven", "cook in this oven"]),
                    ("be_unhungry", ["eat"])
                ], [
                    Intent (have_food 1 1) Nothing,
                    Intent (be_unhungry 1) (Just [eat 1])
                ], bountiful_state
            ),
            (
                [
                    ("have_item", ["take_item 'food' from 4"]),
                    ("be_unhungry", ["eat"])
                ], [
                    Intent (have_food 1 1) (Just (seek_item "food" 1 [1] bountiful_state)),
                    Intent (be_unhungry 1) (Just [eat 1])
                ], bountiful_state
            )
        ]
        
shower :: ([Intent Command State], [Command]) -> State -> String
shower (intents, commands) state = "(" ++ show intents ++ ", " ++ show new_state ++ ")\n"
    where new_state = foldr (\command state -> command state) state commands

assert_equal :: (Show a, Eq a) => a -> a -> IO ()
assert_equal x y = if x == y
                       then putStrLn $ show x ++ " is good"
                       else putStrLn $ show x ++ " /= " ++ show y

main :: IO ()
main = do
    assert_preparations prep_tests
    --let results = map (\(_, initial, state) -> run_preparation initial state) tests
    --fmap assert_equal results
    --map (\(expected, initial, state) -> assert_preparation expected initial state) (return tests)
    --foldr (\(expected, initial, state) world -> assert_preparation expected initial state) tests
