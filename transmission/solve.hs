
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

data NodeType = Sender | Receiver | Broadcaster deriving (Show, Eq)

data Channel = Channel NodeID NodeID deriving (Eq)
instance Show Channel where
    show (Channel (NodeID sourceID) (NodeID destID)) = "{" ++ show sourceID ++ "->" ++ show destID ++ "}"
    
data Node = Node {
    nodeID :: NodeID,
    nodeType :: NodeType,
    inColor :: Color,
    outColor :: Color,
    mana :: Int,
    capacity :: Int,
    starCount :: Int
}
instance Show Node where
    show node =
                "{" ++
                (show $ nodeType node) ++ " " ++
                (show $ nodeID node) ++ " " ++
                (show $ mana node) ++ "/" ++ (show $ capacity node) ++ " " ++
                (show $ outColor node) ++
                (if (starCount node > 0) then (" *" ++ (show $ starCount node) ++ "*") else "") ++
                "}"

instance Equalizer Node where
    isEqual _ _ = True

makeChannels :: [Node] -> [(Int, [Int])] -> [Channel]
makeChannels _ [] = []
makeChannels nodes (r:recipes) = (createChannelPairs sourceID destIDs) ++ (makeChannels nodes recipes)
    where (sourceID, destIDs) = r
          createChannelPairs :: Int -> [Int] -> [Channel]
          createChannelPairs sourceID destIDs = concat (map (\destID -> (createChannelPair (NodeID sourceID) (NodeID destID))) destIDs)
              where createChannelPair :: NodeID -> NodeID -> [Channel]
                    createChannelPair sourceID destID = catMaybes [(tryMakeChannel source dest True), (tryMakeChannel dest source False)]
                      where source = smashJust (getNode nodes sourceID) "couldn't get source in createChannelPair"
                            dest = smashJust (getNode nodes destID) "couldn't get dest in createChannelPair"
                            tryMakeChannel :: Node -> Node -> Bool -> Maybe Channel
                            tryMakeChannel source dest isDirect
                              | (isDirect || canReciprocate (nodeType dest)) = Just (Channel sourceID destID)
                              | otherwise = Nothing
                            canReciprocate Sender = True
                            canReciprocate Receiver = False
                            canReciprocate Broadcaster = False


channel :: Int -> Int -> Channel
channel sourceID destID = if sourceID == destID
    then error $ "channel can't have sourceID = " ++ (show sourceID) ++ " and destID = " ++ (show destID)
    else Channel (NodeID sourceID) (NodeID destID)

receiver nodeID color capacity = Node {
    nodeID = NodeID nodeID,
    nodeType = Receiver,
    inColor = color,
    outColor = Void,
    mana = 0,
    capacity = capacity,
    starCount = 0
}

sender nodeID color mana capacity = starSender nodeID color mana capacity 0

broadcaster nodeID color = Node {
    nodeID = NodeID nodeID,
    nodeType = Broadcaster,
    inColor = color,
    outColor = color,
    mana = 0,
    capacity = 0,
    starCount = 0
}

starSender nodeID color mana capacity starCount = Node {
    nodeID = NodeID nodeID,
    nodeType = Sender,
    inColor = color,
    outColor = color,
    mana = mana,
    capacity = capacity,
    starCount = starCount
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

solveStep :: State -> [State]
solveStep state = map flowLinksRepeated linkedStates
    where linkedStates = map (linkChannel state) chosenChannels
          chosenChannels = chooseChannel state
    
listContains :: Eq a => [a] -> a -> Bool
listContains [] _ = False
listContains (x:xs) element = (x == element) || (listContains xs element)


chooseChannel :: State -> [Channel]
chooseChannel (State _ [] _) = []
chooseChannel (State nodes (c:channels) links)
  | outColor source /= inColor dest = otherChannels
  | mana source <= 0 = otherChannels
  | maxTransferQuantity source dest <= 0 = otherChannels
  | listContains links (Channel destID sourceID) = otherChannels
  | otherwise = c : otherChannels
    where otherChannels = chooseChannel (State nodes channels links)
          Channel sourceID destID = c
          source = smashJust (getNode nodes sourceID) "source missing in chooseChannel"
          dest = smashJust (getNode nodes destID) "dest missing in chooseChannel"

linkChannel :: State -> Channel -> State
linkChannel state channel =
    case (arrayRemove channels channel) of
        Nothing -> error "tried to link non-existent channel"
        Just newChannels -> State nodes newChannels (channel:links)
        where State nodes channels links = state

flowLinksRepeated :: State -> State
flowLinksRepeated state | changed = flowLinksRepeated newState
                        | otherwise = newState
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
    putStrLn $ show $ (solve state)
    putStrLn "---------" 

solvePuzzle initialState = showListExploded "\n" states
                           where states = solve initialState

samplePuzzle2 = State nodes channels []
                  where nodes = [
                                    sender 2 Orange 3 4,
                                    sender 4 Orange 8 8
                                ]
                        channels = [
                                        channel 2 4,
                                        channel 4 2
                                   ]


-- given a state, return a list of all Winning states that it generates
solve :: State -> [State]
solve state = foldl (++) []
                  (map (\s ->
                      let (State nodes _ _) = s in
                          if (winner nodes)
                               then [s]
                               else solve s)
                      (solveStep state))
    

winner :: [Node] -> Bool
winner [] = True
winner (n:nodes) = if (capacity n == 0)
    then winner nodes
    else False
        
puzzle2_1 = State nodes channels []
            where nodes = [
                              sender 1 White 1 0,
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
                              sender 1 White 1 0,
                              sender 2 White 1 0,
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

puzzle3_12 = State nodes channels []
            where nodes = [
                              sender 1 Blue 0 2,
                              sender 2 Blue 1 0,
                              sender 3 Blue 1 1,
                              sender 4 Blue 0 5,
                              sender 5 Blue 0 2,
                              receiver 6 Blue 1,
                              starSender 7 Blue 0 2 1
                          ]
                  channels = makeChannels nodes [
                                 (1, [2, 3, 4, 5, 6]),
                                 (2, [3, 4, 5, 7]),
                                 (3, [4, 6, 7]),
                                 (4, [5, 6, 7]),
                                 (5, [6, 7]),
                                 (6, [7])
                             ]

puzzle4_7 = State nodes channels []
            where nodes = [
                              sender 1 White 0 4,
                              starSender 2 White 0 2 1,
                              sender 3 White 1 2,
                              broadcaster 4 White,
                              sender 5 White 1 1,
                              receiver 6 White 2,
                              sender 7 White 1 1,
                              sender 8 White 0 2
                          ]
                  channels = makeChannels nodes [
                                 (1, [2, 3, 6, 7]),
                                 (2, [3, 4, 5, 7, 8]),
                                 (3, [4, 6, 7]),
                                 (4, [3, 5, 7]),
                                 (5, [4, 6, 7, 8]),
                                 (7, [4, 8])
                             ]


    
main = do
    --solveSamplePuzzle
    
    --let puzzle = samplePuzzle2
    --let puzzle = puzzle2_1
    --let puzzle = puzzle2_3
    --let puzzle = puzzle3_12
    let puzzle = puzzle4_7
    
    putStrLn $ solvePuzzle puzzle
    putStrLn $ show $ length $ solve puzzle
