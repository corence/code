
module Trigger where

import Events

data Trigger = Trigger EventQuery StateQuery [Event]

--damagedCreatureDies = Trigger (EventQuery QueryEventType [EventDamage] Int CardID CardID -- amount, victim, source


-- { "type": "damage", "amount": 3, 
