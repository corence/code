
import Data.Maybe

data Color = Silver | Gold | Blue
data Node = Node
type NodeID = Int
type Link = (NodeID, [NodeID])
type Path = (NodeID, NodeID)

type State = ([Node], [Link], [Path])

nodeID node = 0

getNode :: [Node] -> NodeID -> Node
getNode [] nid = error $ "node " ++ (show nid) ++ " doesn't exist"
getNode (node:nodes) nid
  | (nid == nodeID node) = node
  | otherwise = getNode nodes nid

canSend node = False
canReceive node color = False

colorOfOutput node = Just Silver

-- runStep:
--   find a link with a source node with mana
--     if you can send it, do that!
--       and link the new links!
--   find a Path that could be a Link
--     link it!
runStep :: State -> Maybe State
runStep initialState = do
  let linkToFlow = findLinkToFlow initialState
  if (isJust linkToFlow)
      then Nothing
      else do
          let pathToLink = findPathToLink initialState
          if (isJust pathToLink)
              then Nothing
              else Nothing

-- for each link:
--   is the source willing to send?
--   is at least one dest willing to accept?
--   if yes + yes, return this link
--   otherwise, return any other link
findLinkToFlow :: State -> Maybe Link
findLinkToFlow (_, [], _) = Nothing
findLinkToFlow (nodes, (link:links), paths) =
  if (maybe False anyDestCanReceiveColor (colorOfOutput source))
      then Just link
      else findLinkToFlow (nodes, links, paths)
  where source = getNode nodes sourceID
        dests = map (getNode nodes) destIDs
        (sourceID, destIDs) = link
        anyDestCanReceiveColor color = any (canReceive color) dests

-- for each path:
--   what color is the source able to send?
--   is there a dest willing to accept that color?
--     if yes + yes, return this path
--     otherwise try another path
findPathToLink :: State -> Maybe Path
findPathToLink (_, _, []) = Nothing
findPathToLink (nodes, links, (path:paths)) =
  if (canReceive color dest)
      then Just path
      else findPathToLink (nodes, links, paths)
    where source = getNode nodes sourceID
          dest = getNode nodes destID
          (sourceID, destID) = path
          color = colorOfOutput source
  

main = do
  putStrLn "s"
