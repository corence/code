
--solve state results =
-- | isSuccessful state = state:results
-- | isFailing state = results
-- | otherwise = (solve newState []) : results

{-
class Node t where
    inColor :: Color
    outColor :: Color

instance Node Sender where
    inColor = 

data Node1 = Node1 {
                    id :: Int,
                    nodeType :: NodeType,
                    inColor :: Color,
                    outColor :: Color,
                    mana :: Int,
                    capacity :: Int,
                    channels :: [Channel],
                    links :: [Link]
                 }


                 

data State = State {
                       
                   }



                   -}


data Color = White | Orange | Blue deriving Show

class Equalizer a where
    isEqual :: a -> a -> Bool

instance Equalizer Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False

data Node = Node {
    nodeID :: Int,
    nodeType :: NodeType,
    inColor :: Color,
    outColor :: Color,
    mana :: Int,
    capacity :: Int,
    channels :: [Channel],
    links :: [Link]
}

instance Equalizer Node where
    isEqual _ _ = True

node1 = Node { nodeID = 3, inColor = White }
