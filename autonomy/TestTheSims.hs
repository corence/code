
import Intents
import Actor
import IntentSims
import qualified Data.Map as Map
import Data.Map(Map(..))

intents0 = [Intent (be_unhungry 1) (Just [eat 1])]
state0 = Map.fromList [(1, Actor 1 14 (Map.fromList [("dude", 1)]))]

shower :: ([Intent Command State], [Command]) -> State -> String
shower (intents, commands) state = "(" ++ show intents ++ ", " ++ show new_state ++ ")"
    where new_state = foldr (\command state -> command state) state commands

main = do
    putStrLn $ shower (step_intents intents0 state0) state0
    --putStrLn $ show $ run_actor_step 1 state1
    --putStrLn $ show $ run_actor_step 1 state2
    
