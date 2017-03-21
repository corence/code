
import Intents
import Actor
import IntentSims
import qualified Data.Map as Map
import Data.Map(Map(..))

intents0 = Map.fromList [(1, [Intent (be_unhungry 1) (Just [eat 1])])]

state0 = Map.fromList [(1, Actor 1 14 (Map.fromList [("dude", 1)]))]

-- step :: ActorID -> IntentStacks command state -> state -> (IntentStacks command state, [command])

make_visible :: ActorID -> [Intent Command State] -> State -> String
make_visible aid intents state = show "(" ++ show (Map.size intents) ++ ", " ++ commands_as_string ++ ")"
    where (intents, commands) = step aid intents state
          commands_as_string = show $ map (\command -> command state) commands

main = do
    putStrLn $ make_visible (1 :: Int) intents0 state0
    --putStrLn $ show $ run_actor_step 1 state1
    --putStrLn $ show $ run_actor_step 1 state2
    
