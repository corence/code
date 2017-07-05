
module Threat where

import qualified Pos
import Pos(Pos(..))

type Threat = [Pos]

-- how close a threat goes to its target
distance :: Pos -> Threat -> Int
distance pos threat = minimum $ map (Pos.distance pos) threat
