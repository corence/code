
-- layer 0: Nothing MTG-specific (should be reusable for pokemon or samurai card games, and probably for Uno too)
 -- activated abilities
 -- triggered abilities
 -- cards (concept)
 -- counters (concept)
 -- effects (with a return value and possible failure)
 -- surrender
 -- attachments (this seems suitably general)

data Query = Query

class Action a where
  --lesser :: t -> t -> Bool
  --greater :: t -> t -> Bool

data EventHappens = EventHappens {
}

instance Action EventHappens where
  --lesser x y = x < y
  --greater x y = x > y

data Ability = Ability {
    aCosts :: [Action],
    aResults :: [Action]
}

data Attachment = Attachment {
    aSource :: AbilityID
}

instance Action

main = return ()
