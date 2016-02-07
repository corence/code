
import Data.Maybe
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

arrayRemove :: Eq a => [a] -> a -> Maybe [a]
arrayRemove [] _ = Nothing
arrayRemove (x:xs) q | q == x = Just xs
                     | otherwise = (arrayRemove xs q) >>= (\result -> Just (x:result))
                     

data Color = White | Orange | Blue deriving (Show, Eq)

class Equalizer a where
    isEqual :: a -> a -> Bool

instance Equalizer Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False

data NodeID = NodeID Int deriving (Show, Eq)
data NodeType = Sender | Receiver deriving (Show, Eq)

data Channel = Channel NodeID NodeID deriving (Show, Eq)
data Link = Link Channel deriving (Show, Eq)

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

channel sourceID destID = Channel (NodeID sourceID) (NodeID destID)

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
    
data State = State [Node] [Channel] [Link]

instance Show State where
    show (State nodes channels links) = showNodes ++ "\n" ++ showChannels ++ "\n" ++ showLinks
        where showNodes = "Nodes: " ++ showListExploded nodes
              showChannels = "Channels: " ++ showListExploded channels
              showLinks = "Links: " ++ showListExploded links


showListExploded :: Show a => [a] -> String
showListExploded things = foldl (++) "" (map (\thing -> ("\n    " ++ (show thing))) things)

solve :: State -> Maybe State
--solve state = do
    --channel <- chooseChannel state
    --preFlow <- linkChannel state channel
    --flowLinksRepeated preFlow
    
solve state =
    (chooseChannel state) >>=
    (linkChannel state) >>=
    flowLinksRepeated

chooseChannel :: State -> Maybe Channel
chooseChannel (State _ [] _) = Nothing
chooseChannel state = Just c
    where Channel lhs rhs = c
          State nodes (c:channels) links = state
          
linkChannel :: State -> Channel -> Maybe State
linkChannel state channel = do
    let link = Link channel
    newChannels <- arrayRemove channels channel
    return (State nodes newChannels (link:links))
        where State nodes channels links = state

flowLinksRepeated state | changed = flowLinksRepeated newState
                        | otherwise = Just newState
                        where (newState, changed) = flowLinks state

flowLinks :: State -> (State, Bool)
{-
flowLinks (State nodes channels (link:links)) =
    (arrayRemove nodes source) >>=
    (\nodes1 -> arrayRemove nodes1 dest) >>=
    (\nodes2 -> if transferQuantity > 0
                    then State (newSource : newDest : nodes2) channels (link:links)
                    else flowLinks (State nodes channels links)
                    )
    
        where transferQuantity = max (mana source) (capacityAvailable dest (outColor source))
              source = getNode nodes sourceID
              dest = getNode nodes destID
              Link (Channel sourceID destID) = link
              newSource = source { mana = (mana source) - transferQuantity }
              newDest = dest {
                                mana = (mana dest) + transferQuantity,
                                capacity = (capacity dest) - transferQuantity
                             }
-}

{-
flowLinks :: State -> (State, Bool)
flowLinks (State nodes channels (link:links)) =
    (getNode nodes sourceID) >>=
    (\source -> (getNode nodes destID >>=
        (\dest -> (arrayRemove nodes source >>=
            (\nodes1 -> arrayRemove nodes1 dest) >>=
            (\nodes2 -> (
    
    (arrayRemove nodes source) >>=
    (\nodes1 -> arrayRemove nodes1 dest) >>=
    (\nodes2 -> if transferQuantity > 0
-}

 -- get the source and dest nodes
 -- update their mana levels
 -- if this happened, replace them in the nodes pile
flowLinks state =
    if ((isJust maybeSource) && (isJust maybeDest))
        then if ((transferQuantity > 0))
            then if ((isJust updatedNodes))
                then (updatedState, True)
                else error "updatedNodes didn't exist"
            else (state, False)
        else error "source or dest not found"
        where (State nodes channels (link:links)) = state
              Link (Channel sourceID destID) = link
              maybeSource = getNode nodes sourceID
              maybeDest = getNode nodes destID
              source = fromJust maybeSource
              dest = fromJust maybeDest
              transferQuantity = max (mana source) (capacityAvailable dest (outColor source))
              newSource = source { mana = (mana source) - transferQuantity }
              newDest = dest { mana = (mana dest) + transferQuantity, capacity = (capacity dest) - transferQuantity }
              updatedNodes = (replaceNode sourceID newSource nodes) >>= (replaceNode destID newDest)
              updatedState = State (fromJust updatedNodes) channels (link:links)
              
        
getNode :: [Node] -> NodeID -> Maybe Node
getNode [] _ = Nothing
getNode (node:nodes) nid | nodeID node == nid = Just node
                         | otherwise = getNode nodes nid

replaceNode :: NodeID -> Node -> [Node] -> Maybe [Node]
replaceNode _ _ [] = Nothing
replaceNode targetNid replacementNode (node:nodes)
  | (nodeID node) == targetNid = Just (replacementNode:nodes)
  | otherwise = (replaceNode targetNid replacementNode nodes) >>= (\newNodes -> return (node:newNodes))

capacityAvailable :: Node -> Color -> Int
capacityAvailable destNode color
  | inColor destNode /= color = 0
  | otherwise = capacity destNode

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
                    channel 2 4,
                    channel 4 2
                   ]
    let state = State nodes channels []
    putStrLn $ show state
    putStrLn "---------" 
    putStrLn $ show $ fromJust $ solve state
    putStrLn "---"
    putStrLn "---"
    putStrLn $ show $ arrayRemove [1, 2, 3, 4] 3
