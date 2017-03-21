
import Intents
import Actor
import IntentSims
import qualified Data.Map as Map
import Data.Map(Map(..))

intents0 = Map.fromList [(1, [(be_unhungry 1, Just [eat 1])])]

state0 = Map.fromList [(1, Actor 1 14 (Map.fromList [("dude", 1)]))]

-- step :: ActorID -> IntentStacks command state -> state -> (IntentStacks command state, [command])

main = do
    putStrLn $ show $ step 1 intents0 state0
    --putStrLn $ show $ run_actor_step 1 state1
    --putStrLn $ show $ run_actor_step 1 state2
    
