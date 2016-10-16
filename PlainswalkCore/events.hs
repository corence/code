
main = return ()

--some events:
 --- when a creature has 0 toughness, move it to the graveyard
 --- when a player plays a blue spell, gain 1 life
 --- when you would draw a card, draw 2 cards
 --- when a player would gain life, do nothing

data Board = Board [Card]

data Card = Card {
    cColor :: Int
}

containsColor holder query = holder == query

colorBlue = 3

data Event = Cast {
    card :: Card,
    board :: Board
}

--instance Event Cast where

triggerPlayerCastsBlueSpell :: Event -> Bool
triggerPlayerCastsBlueSpell event = case event of
  Cast card _ -> cColor(card) `containsColor` colorBlue
  otherwise -> False

triggerPlayerWouldGainLife :: Event -> Bool
triggerPlayerWouldGainLife event = case event of
  GainLife player ->
