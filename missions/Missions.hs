{-

Goal:
 - i want to increase my hunger bar
 - i want to decrease my danger bar
 - i want to increase my wealth
 - i want to increase my social standing

Mission:
 - terminal goal: increase social standing
 - therefore-goal: feed everyone
 - potential missions: cook dinner, order pizza
 
 - chosen mission: cook dinner
 - subgoals: obtain prepped materials, stand near stove, channel
 - potential missions: already have, prep materials, find prepped materials
 
 - chosen mission: prep materials
 - subgoals: obtain raw materials, be near surface, channel
 - potential missions: already have, find raw materials, buy from fridge
 
 - chosen mission: buy from fridge
 - goals: obtain cash, be near fridge, convert
 - missions: already have, get a job
 
 - chosen mission: already have


Mission
 - estimate duration and final location: estimateOutcome :: Mission -> World -> Thing -> (Interval, Thing)
 - isComplete? :: Mission -> World -> Thing -> Bool
 - step :: Mission -> World -> Thing -> World

data AssignedMission = AssignedMission {
    AM_owner: Thing,
    AM_briefing :: Mission,
    AM_plan :: Mission
}

MultipleChoiceMission
 - estimateOutcome = chooseBest missions
 - isComplete = any isComplete missions
 - step = step (chooseBest missions)

Beacon = { B_openMissions = [Mission], B_assignedMissions = [AssignedMission] }



-- keep your current mission if you got one
updateMission :: Thing -> Maybe Mission -> Mission
updateMission thing Nothing = selectMission thing
updateMission thing Just x = x


sims:
Mission: Cook dinner
outcomes: +8 dinner objects, -1 preppedMaterials

Mission: Prep dinner materials
outcomes: +1 preppedMaterials, -1 rawMaterials

Mission: Get dinner materials
outcomes: +1 rawMaterials, -20 simoleans
-}

import Data.Map (Map)
import qualified Data.Map as Map

data Mission = Mission

missionGetResources = Mission

{-
missions = fromList [
    ("cookDinner", Mission something),
    ("prepMaterials", Mission something)
]
-}

main = do
    let outcomes = missionOutcomes missionGetResources
    let advancedOutcomes = missionOutcomes missionPrepMaterials
    let omegaOutcomes = missionOutcomes mission
