
import Data.Maybe
import Debug.Trace

--traceRejection x y = trace x y
traceStep s = traceShowId s
traceRejection x y = y
--traceStep s = s

arrayRemove :: Eq a => [a] -> a -> Maybe [a]
arrayRemove [] _ = Nothing
arrayRemove (x:xs) q | q == x = Just xs
                     | otherwise = (arrayRemove xs q) >>= (\result -> Just (x:result))
                     

data Color = White | Orange | Blue | Void deriving (Show, Eq)

data Transfer = Transfer Int Node [Node] deriving Show

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
    show (Channel (NodeID sourceID) (NodeID destID)) = "{" ++ show sourceID ++ "-" ++ show destID ++ "}"
    
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

data Recipe = Recipe NodeID [NodeID]

makeChannels :: [Node] -> [(Int, [Int])] -> [Channel]
makeChannels nodes rawRecipes = makeChannelsForRecipes nodes recipes
                                    where recipes = map makeRecipe rawRecipes

makeRecipe :: (Int, [Int]) -> Recipe
makeRecipe (rawSource, rawDests) = Recipe source dests
                                       where source = NodeID rawSource
                                             dests = map NodeID rawDests

makeChannelsForRecipes :: [Node] -> [Recipe] -> [Channel]
makeChannelsForRecipes nodes recipes = concat (map (makeChannelsForRecipe nodes) recipes)

-- make up to 2 channels for each pair of nodes
makeChannelsForRecipe :: [Node] -> Recipe -> [Channel]
makeChannelsForRecipe nodes recipe = concat (map (makeChannelPairFromIDs nodes sourceID) destIDs)
                                     where Recipe sourceID destIDs = recipe

makeChannelPairFromIDs :: [Node] -> NodeID -> NodeID -> [Channel]
makeChannelPairFromIDs nodes sourceID destID = makeChannelPair source dest
                                                   where source = smashJust (getNode nodes sourceID) "couldn't get source in makeChannelPairFromIDs"
                                                         dest = smashJust (getNode nodes destID) "couldn't get dest in makeChannelPairFromIDs"

makeChannelPair :: Node -> Node -> [Channel]
makeChannelPair source dest = catMaybes [(tryMakeChannel source dest True), tryMakeChannel dest source False]

tryMakeChannel :: Node -> Node -> Bool -> Maybe Channel
tryMakeChannel source dest isDirect
  | isDirect = channel
  | canReciprocate (nodeType source) && canReciprocate (nodeType dest) = channel
  | otherwise = Nothing
      where channel = Just (Channel (nodeID source) (nodeID dest))

canReciprocate :: NodeType -> Bool
canReciprocate Sender = True
canReciprocate Broadcaster = False
canReciprocate Receiver = False

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
              showChannels = "Channels: " ++ showListExploded " " channels
              showLinks = "Links: " ++ showListExploded " " links


showListExploded :: Show a => String -> [a] -> String
showListExploded indent things = (foldr (++) "" (map (\thing -> (indent ++ (show thing))) things)) ++ "\n"

smashJust :: Maybe a -> String -> a
smashJust Nothing message = error message
smashJust (Just q) _ = q

applySolution :: [Channel] -> State -> [State]
applySolution [] state = [state]
applySolution (link:links) state = state : (applySolution links newState)
    where newState = flowLinksRepeated (linkChannel link state)

solveStep :: State -> [State]
solveStep state = map flowLinksRepeated linkedStates
    where linkedStates = map (linkNodes state) chosenChannels
          chosenChannels = chooseChannel state
    
listContains :: Eq a => [a] -> a -> Bool
listContains [] _ = False
listContains (x:xs) element = (x == element) || (listContains xs element)


chooseChannel :: State -> [Channel]
chooseChannel (State _ [] _) = []
chooseChannel (State nodes (c:channels) links)
  | outColor source /= inColor dest = traceRejection "color" otherChannels
  | (mana source <= 0) && (nodeType source == Sender) = traceRejection "mana" otherChannels
  | maxTransferQuantity source dest <= 0 = traceRejection "quant" otherChannels
  | listContains links (Channel destID sourceID) = traceRejection "backlink" otherChannels
  | otherwise = c : otherChannels
    where otherChannels = chooseChannel (State nodes channels links)
          Channel sourceID destID = c
          source = smashJust (getNode nodes sourceID) "source missing in chooseChannel"
          dest = smashJust (getNode nodes destID) "dest missing in chooseChannel"

linkNodes :: State -> Channel -> State
linkNodes state channel = case (nodeType dest) of
  Sender -> sourcedState
  Receiver -> sourcedState
  Broadcaster -> traceRejection ("ok mf, ss: " ++ (show sourcedState) ++ "\n\n destChannels: " ++ (show destChannels) ++ ", sourcedChannels=" ++ (show sourcedChannels) ++ ", destID=" ++ (show destID)) $ foldr linkChannel sourcedState destChannels
  where Channel sourceID destID = channel
        dest = grabNode nodes destID
        State nodes channels links = state
        sourcedState = linkChannel channel state
        destChannels = channelsFrom destID sourcedChannels
        sourcedChannels = chooseChannel sourcedState

channelsFrom :: NodeID -> [Channel] -> [Channel]
channelsFrom sourceID channels = filter (\(Channel nid _) -> sourceID == nid) channels

linkChannel :: Channel -> State -> State
linkChannel channel state =
    case (arrayRemove channels channel) of
        Nothing -> error "tried to link non-existent channel"
        Just newChannels -> case (arrayRemove channels (invertChannel channel)) of
                                Nothing -> State nodes newChannels (channel:links)
                                Just leanChannels -> State nodes leanChannels (channel:links)
        where State nodes channels links = state

invertChannel :: Channel -> Channel
invertChannel (Channel x y) = Channel y x

flowLinksRepeated :: State -> State
flowLinksRepeated state = case (tryFlow state) of
                        Just newState -> flowLinksRepeated newState
                        Nothing -> trace ("flowLinksRepeated just stopped") state

tryFlow :: State -> Maybe State
tryFlow state
  | length transfers > 0 = trace ("splashing dests " ++ (show dests)) (Just $ foldr splashLinks (State newNodes channels links) dests)
  | otherwise = Nothing
    where State nodes channels links = state
          transfers = catMaybes (map (tryFlowNode state) nodes)
          transfer = head transfers
          Transfer _ _ dests = transfer
          newNodes = applyTransfer nodes transfer

splashLinks :: Node -> State -> State
splashLinks target state
  | nodeType target == Broadcaster = foldr linkChannel state targetsChannels
  | otherwise = state
    where targetsChannels = trace ("channelsFrom (" ++ (show $ nodeID target) ++ ") (chooseChannel " ++ (show state)) (channelsFrom (nodeID target) (chooseChannel state))

tryFlowNode :: State -> Node -> Maybe Transfer
tryFlowNode state source = case (nodeType source) of
  Sender -> trySendToAnyLink state source
  Broadcaster -> tryBroadcast state source
  Receiver -> Nothing

tryBroadcast :: State -> Node -> Maybe Transfer
tryBroadcast state source
  | mana source <= 0 = Nothing
  | otherwise = Just (Transfer (mana source) source dests)
      where State nodes channels links = state
            sourceLinks = channelsFrom (nodeID source) links
            destIDs = map (\(Channel _ destID) -> destID) sourceLinks
            dests = map (grabNode nodes) destIDs

tryFlowToDest :: Node -> Node -> (Int, Node)
tryFlowToDest source dest = (transferQuantity, newDest)
    where transferQuantity = maxTransferQuantity source dest
          newDest = dest { mana = (mana dest) + transferQuantity, capacity = (capacity dest) - transferQuantity }


trySendToAnyLink :: State -> Node -> Maybe Transfer
trySendToAnyLink state source
  | length goodDests > 0 = Just (Transfer quantity source [dest])
  | otherwise = Nothing
      where State nodes channels links = state
            sourceLinks = channelsFrom (nodeID source) links
            destIDs = map (\(Channel _ destID) -> destID) sourceLinks
            dests = map (grabNode nodes) destIDs
            goodDests = filter (\dest -> maxTransferQuantity source dest > 0) dests
            dest = (head goodDests) :: Node
            quantity = maxTransferQuantity source dest

applyTransfer :: [Node] -> Transfer -> [Node]
applyTransfer nodes (Transfer quantity source dests) = transferFrom quantity source destedNodes
    where destedNodes = foldr (transferTo quantity) nodes dests

-- values.reduce(function (kindler, value) {
--     return kindler.call(value);
-- }, initKindler);

-- foldr (function) initKindler values

transferFrom :: Int -> Node -> [Node] -> [Node]
transferFrom quantity source nodes = replaceNode newSource nodes
    where newSource = source { mana = (mana source) - quantity }

transferTo :: Int -> Node -> [Node] -> [Node]
transferTo quantity dest nodes = if (capacity newDest < 0) then error ("not enough capacity in " ++ (show newDest) ++ " from " ++ (show dest)) else replaceNode newDest nodes
    where newDest = dest { mana = (mana dest) + quantity, capacity = (capacity dest) - quantity }

        
maxTransferQuantity :: Node -> Node -> Int
maxTransferQuantity source dest = min (mana source) (capacityAvailable dest (outColor source))

grabNode :: [Node] -> NodeID -> Node
grabNode nodes nid = smashJust (getNode nodes nid) ("assumption failed! can't grab node " ++ (show nid) ++ " from nodes " ++ (show nodes))
              
getNode :: [Node] -> NodeID -> Maybe Node
getNode [] _ = Nothing
getNode (node:nodes) nid | nodeID node == nid = Just node
                         | otherwise = getNode nodes nid

replaceNodes :: [Node] -> [Node] -> [Node]
replaceNodes [] nodes = nodes
replaceNodes (r:replacementNodes) nodes = replaceNodes replacementNodes (replaceNode r nodes)

replaceNode :: Node -> [Node] -> [Node]
replaceNode node [] = error ("can't find node " ++ (show node) ++ " for replaceNode")
replaceNode replacementNode (node:nodes)
  | (nodeID node) == (nodeID replacementNode) = replacementNode:nodes
  | otherwise = node:(replaceNode replacementNode nodes)

capacityAvailable :: Node -> Color -> Int
capacityAvailable destNode color
  | inColor destNode /= color = 0
  | nodeType destNode == Broadcaster = 999999
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
solve state = foldr (++) []
                  (map (\s ->
                      let (State nodes _ _) = s in
                          if (winner nodes)
                               then [s]
                               else solve s)
                      (solveStep (traceStep state)))
    

winner :: [Node] -> Bool
winner [] = True
winner (n:nodes) = if (capacity n <= 0)
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

puzzle4_1_rc = State nodes channels []
            where nodes = [
                              sender 1 White 1 0,
                              broadcaster 2 White,
                              receiver 3 White 1,
                              receiver 4 White 1
                          ]
                  channels = makeChannels nodes [
                                 (1, [2, 3]),
                                 (2, [1, 3, 4])
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

-- a solution: {2-7} {5-6} {1-2} {3-4} {3-8} {4-9}
puzzle4_9 = State nodes channels []
            where nodes = [
                              sender 1 White 0 1,
                              sender 2 White 1 1,
                              sender 3 White 0 4,
                              sender 4 White 1 1,
                              sender 5 White 0 2,
                              broadcaster 6 White,
                              broadcaster 7 White,
                              receiver 8 White 2,
                              receiver 9 White 2
                          ]
                  channels = makeChannels nodes [
                                 (1, [2, 3, 4, 5, 6, 7, 8]),
                                 (2, [3, 5, 6, 7, 8, 9]),
                                 (3, [4, 5, 6, 7, 8, 9]),
                                 (4, [5, 8, 9]),
                                 (5, [6, 7, 9]),
                                 (6, [1, 3]),
                                 (7, [3, 5])
                             ]

showSolution states = showListExploded "\n" states
    
main = do
    --solveSamplePuzzle
    
    --let puzzle = samplePuzzle2
    --let puzzle = puzzle2_1
    --let puzzle = puzzle2_3
    --let puzzle = puzzle3_12
    --let puzzle = puzzle4_7
    let puzzle = puzzle4_9
    --let puzzle = puzzle4_1_rc
    
    let solution = solve puzzle
    --let solution = applySolution [(channel 2 7), (channel 5 6), (channel 1 2), (channel 3 4), (channel 3 8), (channel 4 9)] puzzle4_9
    putStrLn $ showSolution solution
    putStrLn $ "Solutions: " ++ (show $ length $ solution)
    putStrLn $ "Winners: " ++ (show $ length $ filter (\(State nodes _ _) -> winner nodes) solution)
