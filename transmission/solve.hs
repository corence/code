
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
    capacity :: Int
} deriving Show

instance Equalizer Node where
    isEqual _ _ = True

sender nodeID color mana capacity = Node {
    nodeID = NodeID nodeID,
    nodeType = Sender,
    inColor = color,
    outColor = color,
    mana = mana,
    capacity = capacity
}

converter nodeID inColor outColor mana capacity = (sender nodeID inColor mana capacity ) {outColor = outColor}

node2 = sender 3 White 4 2
node3 = converter 3 White Orange 4 2
    
data State = State [Node] [Channel] [Link] deriving Show

solve :: State -> Maybe State
solve state = do
    channel <- chooseChannel state
    preFlow <- linkChannel state channel
    flowLinks preFlow

flowLinks state = Just state
linkChannel state channel = Just state

chooseChannel :: State -> Maybe Channel
chooseChannel (State _ [] _) = Nothing
chooseChannel state = Just c
    where Channel lhs rhs = c
          State nodes (c:channels) links = state

newState = State [] [] []
addNode node (State nodes channels links) = State (node:nodes) channels links
addChannel (State nodes channels links) channel = State nodes (channel:channels) links
addLink (State nodes channels links) link = State nodes channels (link:links)

main = do
    let nodes = [
                    sender 2 White 3 4,
                    sender 4 Orange 8 8
                ]
    let channels = [
                    Channel 2 4,
                    Channel 4 2
                   ]
    let state = State nodes channels []
    print $ show $ solve state
