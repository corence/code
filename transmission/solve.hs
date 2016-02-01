
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

data NodeID = NodeID Int deriving Show
data NodeType = Sender | Receiver deriving Show

data Channel = Channel Int Int deriving Show
data Link = Link Channel deriving Show

data Node = Node {
    nodeID :: NodeID,
    nodeType :: NodeType,
    inColor :: Color,
    outColor :: Color,
    mana :: Int,
    capacity :: Int,
    channels :: [Channel],
    links :: [Link]
} deriving Show

instance Equalizer Node where
    isEqual _ _ = True

sender nodeID color mana capacity channels = Node {
    nodeID = NodeID nodeID,
    nodeType = Sender,
    inColor = color,
    outColor = color,
    mana = mana,
    capacity = capacity,
    channels = channels,
    links = []
}

converter nodeID inColor outColor mana capacity channels = (sender nodeID inColor mana capacity channels) {outColor = outColor}

node2 = sender 3 White 4 2 []
node3 = converter 3 White Orange 4 2 []
    
data State = State [Node] [Channel] [Link] deriving Show

solve state = result
  where State nodes channels links = state
        channel = chooseChannel channels
        state1 = linkChannel state channel
        result = flowLinks state1

flowLinks state = state
linkChannel state channel = state
chooseChannel (c:channels) = c

newState = State [] [] []
addNode (State nodes channels links) node = State (node:nodes) channels links
addChannel (State nodes channels links) channel = State nodes (channel:channels) links
addLink (State nodes channels links) link = State nodes channels (link:links)

main = do
    let state = newState
    print $ show $ solve state
