
import Data.Maybe

arrayRemove :: Eq a => [a] -> a -> Maybe [a]
arrayRemove [] _ = Nothing
arrayRemove (x:xs) q | q == x = Just xs
                     | otherwise = (arrayRemove xs q) >>= (\result -> Just (x:result))
                     

data Color = White | Orange | Blue | Void deriving (Show, Eq)

class Equalizer a where
    isEqual :: a -> a -> Bool

instance Equalizer Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False

data NodeID = NodeID Int deriving (Eq)
instance Show NodeID where
    show (NodeID nid) = "#" ++ (show nid)

data NodeType = Sender | Receiver deriving (Show, Eq)

data Channel = Channel NodeID NodeID deriving (Eq)
instance Show Channel where
    show (Channel (NodeID sourceID) (NodeID destID)) = "{" ++ show sourceID ++ "->" ++ show destID ++ "}"
    
data Node = Node {
    nodeID :: NodeID,
    nodeType :: NodeType,
    inColor :: Color,
    outColor :: Color,
    mana :: Int,
    capacity :: Int
}
instance Show Node where
    show node =
                "{" ++
                (show $ nodeType node) ++ " " ++
                (show $ nodeID node) ++ " " ++
                (show $ mana node) ++ "/" ++ (show $ capacity node) ++ " " ++
                (show $ outColor node) ++
                "}"

instance Equalizer Node where
    isEqual _ _ = True

channel sourceID destID = Channel (NodeID sourceID) (NodeID destID)

receiver nodeID color capacity = Node {
    nodeID = NodeID nodeID,
    nodeType = Receiver,
    inColor = color,
    outColor = Void,
    mana = 0,
    capacity = capacity
}

sender nodeID color mana capacity = Node {
    nodeID = NodeID nodeID,
    nodeType = Sender,
    inColor = color,
    outColor = color,
    mana = mana,
    capacity = capacity
}

converter nodeID inColor outColor mana capacity = (sender nodeID inColor mana capacity ) {outColor = outColor}

data State = State [Node] [Channel] [Channel]

instance Show State where
    show (State nodes channels links) = showNodes ++ showChannels ++ showLinks
        where showNodes = "Nodes: " ++ showListExploded "\n    " nodes
              showChannels = "Channels: " ++ showListExploded "\n    " channels
              showLinks = "Links: " ++ showListExploded "\n    " links


showListExploded :: Show a => String -> [a] -> String
showListExploded indent things = (foldl (++) "" (map (\thing -> (indent ++ (show thing))) things)) ++ "\n"

smashJust :: Maybe a -> String -> a
smashJust Nothing message = error message
smashJust (Just q) _ = q

solve :: State -> State
solve state | isJust newState = fromJust newState
            | otherwise = state
            where newState = solveStep state

solveStep :: State -> Maybe State
solveStep state =
    (chooseChannel state) >>=
    (linkChannel state) >>=
    flowLinksRepeated

chooseChannel :: State -> Maybe Channel
chooseChannel (State _ [] _) = Nothing
chooseChannel state
  | colorsMatch && willTransferNonZeroAmount = Just c
  | otherwise = chooseChannel (State nodes channels links)
    where Channel sourceID destID = c
          source = smashJust (getNode nodes sourceID) "source missing in chooseChannel"
          dest = smashJust (getNode nodes destID) "dest missing in chooseChannel"
          colorsMatch = (outColor source) == (inColor dest)
          State nodes (c:channels) links = state
          willTransferNonZeroAmount = maxTransferQuantity source dest > 0
          
linkChannel :: State -> Channel -> Maybe State
linkChannel state channel = do
    newChannels <- arrayRemove channels channel
    return (State nodes newChannels (channel:links))
        where State nodes channels links = state

flowLinksRepeated state | changed = flowLinksRepeated newState
                        | otherwise = Just newState
                        where (newState, changed) = flowLinks state

flowLinks :: State -> (State, Bool)
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
              Channel sourceID destID = link
              maybeSource = getNode nodes sourceID
              maybeDest = getNode nodes destID
              source = smashJust maybeSource "source missing in flowLinks"
              dest = smashJust maybeDest "dest missing in flowLinks"
              transferQuantity = maxTransferQuantity source dest
              newSource = source { mana = (mana source) - transferQuantity }
              newDest = dest { mana = (mana dest) + transferQuantity, capacity = (capacity dest) - transferQuantity }
              updatedNodes = (replaceNode sourceID newSource nodes) >>= (replaceNode destID newDest)
              updatedState = State (smashJust updatedNodes "Nodes didn't update in flowLinks") channels (link:links)
              
        
maxTransferQuantity :: Node -> Node -> Int
maxTransferQuantity source dest = min (mana source) (capacityAvailable dest (outColor source))
              
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
    --solveSamplePuzzle
    --putStrLn solveRealPuzzle
    putStrLn $ solvePuzzle puzzle2_1
    --putStrLn $ solvePuzzle puzzle2_3

puzzle2_1 = State nodes channels []
            where nodes = [
                              sender 1 White 1 1,
                              sender 2 White 0 1,
                              receiver 3 White 1
                          ]
                  channels = [
                                 channel 1 2,
                                 channel 1 3,
                                 channel 2 1,
                                 channel 2 3
                             ]

puzzle2_3 = State nodes channels []
            where nodes = [
                              sender 1 White 1 1,
                              sender 2 White 1 1,
                              sender 3 White 0 2,
                              receiver 4 White 1,
                              receiver 5 White 1
                          ]
                  channels = [
                                 channel 1 2,
                                 channel 1 3,
                                 channel 1 4,
                                 channel 2 1,
                                 channel 2 3,
                                 channel 2 5,
                                 channel 3 1,
                                 channel 3 2,
                                 channel 3 4,
                                 channel 3 5
                             ]
    
solveSamplePuzzle = do
    let nodes = [
                    sender 2 Orange 3 4,
                    sender 4 Orange 8 8
                ]
    let channels = [
                    channel 2 4,
                    channel 4 2
                   ]
    let state = State nodes channels []
    putStrLn $ show state
    putStrLn "---------" 
    putStrLn $ show $ smashJust (solveStep state) "State didn't get solved in main"
    putStrLn "---------" 

solvePuzzle initialState = showListExploded "\n" states
                           where states = solveGood initialState

solveRealPuzzle = solvePuzzle initialState
                  where initialState = State nodes channels []
                        nodes = [
                                    sender 2 Orange 3 4,
                                    sender 4 Orange 8 8
                                ]
                        channels = [
                                        channel 2 4,
                                        channel 4 2
                                   ]


solveGood :: State -> [State]
solveGood state = case (solveStep state) of
    Nothing -> [state]
    Just s -> state : solveGood s
