
import Data.Maybe

data Color = Silver | Gold | Blue
data Node = Node
type NodeID = Int
type Link = (NodeID, [NodeID])
type Path = (NodeID, NodeID)

type State = ([Node], [Link], [Path])

nodeID node = 0
nodeSend node = (Silver, node)
nodeReceive :: Color -> Node -> (Node, Maybe Link)
nodeReceive color node = (node, Nothing)

getNode :: [Node] -> NodeID -> Node
getNode [] nid = error $ "node " ++ (show nid) ++ " doesn't exist"
getNode (node:nodes) nid
  | (nid == nodeID node) = node
  | otherwise = getNode nodes nid

replaceNode :: [Node] -> NodeID -> Node -> [Node]
replaceNode [] nid _ = error $ "node " ++ (show nid) ++ " not found for replacing"
replaceNode (node:nodes) nid replacement
  | nodeID node == nid = replacement : nodes
  | otherwise = node : (replaceNode nodes nid replacement)

linkContainsPath :: Link -> Path -> Bool
linkContainsPath (linkSourceID, linkDestIDs) (pathSourceID, pathDestID)
  | (pathSourceID == linkSourceID) && (pathDestID `elem` linkDestIDs) = True
  | (pathDestID == linkSourceID) && (pathSourceID `elem` linkDestIDs) = True
  | otherwise = False

flowAnyLink :: State -> Maybe State
flowAnyLink state = maybe Nothing (Just $ flowLink state) (findFlowableLink state)

-- flowable: source has mana, dest can receive mana
findFlowableLink :: State -> Maybe Link
findFlowableLink (nodes (link:links) _)
  | 

flowLink :: State -> Link -> State
flowLink state (sourceID, destIDs) = foldr (flowToDest color) state2 (map (getNode nodes2) destIDs)
    where (color, state2) = flowFromSource state source
          source = getNode nodes sourceID
          (nodes, _, _) = state
          (nodes2, _, _) = state2

flowFromSource :: State -> Node -> (Color, State)
flowFromSource (nodes, links, paths) source = (color, ((replaceNode nodes sourceID newSource), links, paths))
    where (color, newSource) = nodeSend source
          sourceID = nodeID source

flowToDest :: Color -> Node -> State -> State
flowToDest color dest (nodes, links, paths) = ((replaceNode nodes destID newDest), newLinks, newPaths)
    where (newDest, maybeNewLink) = nodeReceive color dest
          destID = nodeID dest
          newLinks = maybe links (\newLink -> newLink : links) maybeNewLink
          newPaths = maybe paths (pathsWithoutLink paths) maybeNewLink
          pathsWithoutLink :: [Path] -> Link -> [Path]
          pathsWithoutLink paths = (\newLink -> filter (\path -> not (linkContainsPath newLink path)) paths)


main = do
  putStrLn "beam2"
