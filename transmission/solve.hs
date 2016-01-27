
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

data NodeID = NodeID Int
data NodeType = Sender | Receiver

data Channel = Channel Int Int
data Link = Link Channel

data Node = Node {
    nodeID :: NodeID,
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

node1 = Node { nodeID = (NodeID 3), inColor = White }

data State = State [Node]


solve state = solve state2
    where selectedChannel = chooseChannel state,
          state1 = linkChannel selectedChannel state,
          state2 = flowLinks state1
          
