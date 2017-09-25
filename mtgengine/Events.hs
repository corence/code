
{-# LANGUAGE OverloadedStrings #-}

module Events where

import Event

{-
Bob
Creature - Zombie
Black/Red
6/6
Whenever a black creature would receive 4 or greater damage from a green source, if it has haste, then destroy it instead.

Caroline
Creature - Ghoul
Green/White
4/6
Whenever a multicoloured creature receives combat damage, if 2 or more green creatures have died this turn, then this creature gains +0/+X until end of turn, where X is the amount of combat damage.

Elfgorger
Artifact Creature - Goblin
1/1
When an Aura is attached to a player, if that player has an Elf permanent, then that player creates a copy of each Goblin permanent on the battlefield.

Mountain of Redirection
Land Creature - Wizard Mountain
2/2
When this enters the battlefield, target Aura becomes attached to target Permanent. (Assignments must be legal.)
Tap: Add R to your mana pool.
-}

bobZombiesTrigger = Trigger True query resolver
    where query = EventQueryDamage
          resolver _ event@(EventDamage isCombat amount victim source)
              = if condition board && interstitial board
                    then [eventDestroy victim & conditionalEvent interstitial & eventToStack & eventToTriggerPool]
                    else [event]
          condition board = amount >= 4 && hasColor Green source board
          interstitial board = hasAttribute Haste victim board

-- Whenever a multicoloured creature receives combat damage, if 2 or more green creatures have died this turn, then this creature gains +0/+X until end of turn, where X is the amount of combat damage.
carolineTrigger = Trigger False query resolver
    where query = EventQueryDamage
          resolver board event@(EventDamage isCombat amount victim source)
              = if condition board && interstitial board
                  then eventPump 0 amount & conditionalEvent interstitial & eventUntilEndOfTurn
                  else []
          condition board = isCombat && length (color victim board) > 1
          interstitial board = queryEvents (currentTurn board) board EventQueryMove & filter (\(EventMove subject destination source) -> isColor [Green] True subject board && isCreature subject board && source == battlefield && destination == graveyard) & (\events -> length events >= 2) -- TODO: each event should probably have a copy of the board at that time -- or else perhaps it should store concrete creatures, not just their IDs. Otherwise how do we check if a creature that died was green at that time?

-- When an Aura is attached to a player, if that player has an Elf permanent, then that player creates a copy of each Goblin permanent on the battlefield.
gorgerTrigger = Trigger False EventAttach resolver
    where resolver board event@(EventAttach player _ child)
            = if condition && interstitial
                  then (\board -> board & takePermanents & takeBySubtype True [SubtypeGoblin] & map (EventClone player) & map (conditionalEvent interstitial) & map eventToStack & map eventToTriggerPool)
                  else []
          condition board = isPlayer player board && hasSubtype SubtypeAura child board
          interstitial board = board & takePermanents & takeBySubtype True [SubtypeElf] & (\elves -> length elves > 0)
