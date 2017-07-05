
module Demon where

import Pos(Pos(..))
import Threat
import World
import qualified Pos

class Demon demon where
    allPossibleThreats :: Demon demon => demon -> [Threat]

data Wizard = Wizard Pos Int -- pos, cooldown

-- threats of a wizard:
--  * generate each possible beam
--  * if it has an obstacle (temple) in it, cut it short
--  * if it has no hoplite in it, delete it
--  * if it has a demon in it, delete it
instance Demon Wizard where
    threats wizard = viableThreats
        where viableThreats = filter (negate . hasDemon) targetfulThreats
              targetfulThreats = filter hasHoplite worldThreats
              worldThreats = map 

viableThreats :: World -> [Threat] -> [Threat]
