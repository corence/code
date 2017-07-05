
module Status where

import qualified Data.Map as Map
import Pos

class Demon a where
    --move :: Demon a => a -> pos -> Action

data Status = Status Board Hoplite [Thing]
data Action = Action String (Status -> Status)

data Board = Map Pos (Terrain, Maybe Thing, Maybe Item)

data Thing = Thing ThingType Pos
data ThingType = Fighter | Archer | Demo1 | Demo2 | Demo3 | Wizard1 | Wizard2 | Shrine | Bomb
data Terrain = Floor | Stairs | Portal
data Item = Item ItemType Pos
data ItemType = Fleece | Spear

data Hoplite = Hoplite Pos Health Shield Boots Quiver
data Health = Health Int Int -- hp, maxhp
data Shield = Shield Int -- cooldown
data Boots = Boots Int Int -- mana, max
data Quiver = Quiver (Maybe Pos) -- if the spear is fallen, where is it

update :: Status -> Status
update status = undefined
-- 1) hoplite action
-- 2) bombs burst
-- 3) demons fall in lava or off the edge
-- 4) demons move, unless they don't

runHopliteAction :: Status -> Status
runHopliteAction = undefined
-- 1) do lunge
-- 2) do stabs

updateThing :: Status -> Thing -> Action
updateThing status (Thing Fighter pos) = undefined -- attack, move, stay
updateThing status (Thing Archer pos) = undefined -- attack, move, stay
updateThing status (Thing Demo1 pos) = undefined -- move, stay
updateThing status (Thing Demo2 pos) = undefined -- move, stay
updateThing status (Thing Demo3 pos) = undefined -- attack, move, stay
updateThing status (Thing Wizard1 pos) = undefined -- attack, move, stay
updateThing status (Thing Wizard2 pos) = undefined -- recharge
updateThing status (Thing Shrine pos) = undefined -- stay
updateThing status (Thing Bomb pos) = undefined -- explode

updateDemon status (Thing Fighter pos) =
    case pathfind pos ((hoplitePos . hoplite) status) of
        Nothing -> stay
        Just path -> if length path == 1
                         then attack
                         else if null path
                             then stay -- this shouldn't really happen in practice
                             else move (head path)
updateDemon status (Thing Archer pos) =
    if canAttack (Thing Archer pos) ((hoplitePos . hoplite) status)
        then attack ((hoplitePos . hoplite) status)
        else move (bestPosition newPositions)
            where newPositions = scanForFiringPositions (Thing Archer pos) status

type FiringPosition = ()
scanForFiringPositions :: Thing -> Status -> [FiringPosition]
scanForFiringPositions (Thing Archer pos) status = map something neighboursWithSelf
    where something pos = ()

pathfind :: Pos -> Pos -> Maybe [Pos]
pathfind startPos goalPos = undefined

stay = Action "stay" id
attack = Action "attack" id
move target = Action "move" id

hoplite :: Status -> Hoplite
hoplite (Status _ hoplite _) = hoplite

hoplitePos :: Hoplite -> Pos
hoplitePos (Hoplite pos _ _ _ _) = pos

-- fighter ai:
-- 1) attack if i'm adjacent to target
-- 2) 
