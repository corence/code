
module Event where

import Board

type EventResolver = (Board -> Event -> ([Event]))
data Trigger = Trigger Bool EventQuery EventResolver -- the bool is "is this a replacement effect". If true, it is triggered when the event is about to happen. If false, then it isn't triggered until the event really happens
