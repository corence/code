{-
A series of three rooms are connected to each other by automatic doors.

p327_rooms_of_doom.gif
Each door is operated by a security card. Once you enter a room the door automatically closes and that security card cannot be used again. A machine at the start will dispense an unlimited number of cards, but each room (including the starting room) contains scanners and if they detect that you are holding more than three security cards or if they detect an unattended security card on the floor, then all the doors will become permanently locked. However, each room contains a box where you may safely store any number of security cards for use at a later stage.

If you simply tried to travel through the rooms one at a time then as you entered room 3 you would have used all three cards and would be trapped in that room forever!

However, if you make use of the storage boxes, then escape is possible. For example, you could enter room 1 using your first card, place one card in the storage box, and use your third card to exit the room back to the start. Then after collecting three more cards from the dispensing machine you could use one to enter room 1 and collect the card you placed in the box a moment ago. You now have three cards again and will be able to travel through the remaining three doors. This method allows you to travel through all three rooms using six security cards in total.

It is possible to travel through six rooms using a total of 123 security cards while carrying a maximum of 3 cards.

Let C be the maximum number of cards which can be carried at any time.

Let R be the number of rooms to travel through.

Let M(C,R) be the minimum number of cards required from the dispensing machine to travel through R rooms carrying up to a maximum of C cards at any time.

For example, M(3,6)=123 and M(4,6)=23.
And, ΣM(C,6)=146 for 3 ≤ C ≤ 4.

You are given that ΣM(C,10)=10382 for 3 ≤ C ≤ 10.

Find ΣM(C,30) for 3 ≤ C ≤ 40.
-}

cardsRequired :: Integer -> Integer -> Integer
cardsRequired capacity numDoors
    | numDoors <= capacity = numDoors -- once we're down to a manageable number of doors, roll straight through
    | otherwise = transportCost capacity (cardsRequired capacity (numDoors - 1))
    where transportCost capacity numCards -- how much does it cost to get that many cards through one door?
            | numCards <= (capacity - 1) = numCards + 1 -- if we can carry all of the remaining cards, plus one to get through the door, let's finish this now and go through
            | otherwise = capacity + (transportCost capacity (numCards - capacity + 2)) -- if not, then we take a full armload of cards through -- but subtract 2 cards to get in and out of the door

sumCardsRequired :: Integer -> Integer -> Integer -> Integer
sumCardsRequired minCapacity maxCapacity numDoors
  = sum $ map (\capacity -> cardsRequired capacity numDoors) [minCapacity..maxCapacity]

assertion :: (Show a, Eq a) => a -> a -> IO ()
assertion x y = if x /= y 
                then putStrLn ("ouch, " ++ show x ++ " != " ++ show y) 
                else putStrLn "ok"

main = do
    assertion 1 $ cardsRequired 4 1
    assertion 2 $ cardsRequired 4 2
    assertion 3 $ cardsRequired 4 3
    assertion 3 $ cardsRequired 3 3
    assertion 6 $ cardsRequired 3 4
    assertion 15 $ cardsRequired 3 5
    assertion 123 $ cardsRequired 3 7
    assertion 23 $ cardsRequired 4 7
    assertion 146 $ sumCardsRequired 3 4 7
    assertion 10382 $ sumCardsRequired 3 10 11
    putStrLn $ show $ cardsRequired 3 3
    putStrLn $ show $ cardsRequired 3 4
    putStrLn $ show $ cardsRequired 3 5
    putStrLn $ show $ cardsRequired 3 6
    putStrLn $ show $ cardsRequired 3 7
    putStrLn $ show $ cardsRequired 3 8
    putStrLn $ show $ cardsRequired 3 9
    putStrLn $ show $ cardsRequired 3 10
    putStrLn "---"
    putStrLn $ show $ cardsRequired 3 31
    putStrLn $ show $ cardsRequired 4 31
    putStrLn $ show $ cardsRequired 5 31
    putStrLn $ show $ cardsRequired 6 31
    putStrLn $ show $ cardsRequired 7 31
    putStrLn $ show $ cardsRequired 8 31
    putStrLn $ show $ cardsRequired 9 31
    --putStrLn $ show $ sumCardsRequired 3 40 31

--And, ΣM(C,6)=146 for 3 ≤ C ≤ 4.
--You are given that ΣM(C,10)=10382 for 3 ≤ C ≤ 10.
--Find ΣM(C,30) for 3 ≤ C ≤ 40.
