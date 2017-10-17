
import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><), (<|), (|>))
import Data.List
import Data.Maybe
import Control.Applicative
import Debug.Trace

(&) = flip ($)

insertMaybe :: Ord k => Maybe a -> k -> Map k a -> Map k a
insertMaybe Nothing key = Map.delete key
insertMaybe (Just value) key = Map.insert key value

data Chain = Chain (Maybe Value) (Seq Pos) deriving (Show, Eq)

data Pos = Pos Int Int deriving (Eq, Ord)
instance Show Pos where
    show (Pos x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data Direction = Nowhere | North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Eq)

instance Show Direction where
    show Nowhere = "--"
    show North = "N "
    show Northeast = "NE"
    show East = " E"
    show Southeast = "SE"
    show South = "S "
    show Southwest = "SW"
    show West = " W"
    show Northwest = "NW"

type Value = Int

data Action = Link String Pos Pos | Unlink String Pos Pos deriving (Show, Eq)

data Status = Status {
    chainsStartingWith :: Map Pos (Seq Chain),
    chainsEndingWith :: Map Pos (Seq Chain),
    directions :: Map Pos Direction,
    possibilities :: [(Pos, Pos)]
    }

sample :: [(Pos, Direction, Maybe Value)]
sample = [
    (Pos 0 0, East, Just 1),
    (Pos 1 0, Southwest, Nothing),
    (Pos 2 0, Southwest, Nothing),
    (Pos 0 1, East, Nothing),
    (Pos 1 1, North, Nothing),
    (Pos 2 1, Southwest, Nothing),
    (Pos 0 2, Northeast, Nothing),
    (Pos 1 2, East, Nothing),
    (Pos 2 2, Nowhere, Just 9)
    ]

straightSample :: [(Pos, Direction, Maybe Value)]
straightSample = [
    (Pos 0 0, East, Just 1),
    (Pos 1 0, East, Nothing),
    (Pos 2 0, East, Nothing)
    ]

makeGrid :: [(Pos, Direction, Maybe Value)] -> Map Pos Direction
makeGrid = foldl' insertDatum Map.empty
    where insertDatum grid (pos, direction, _) = Map.insert pos direction grid

makeChains :: [(Pos, Direction, Maybe Value)] -> Map Pos (Seq Chain)
makeChains = foldl' addChain Map.empty
    where makeChain (pos, _, mvalue) = Chain mvalue (Seq.singleton pos)
          addChain result datum = Map.insert (datumPos datum) (Seq.singleton $ makeChain datum) result
          datumPos (pos, _, _) = pos

makePossibilities :: Map Pos Direction -> [(Pos, Pos)]
makePossibilities grid = Map.foldrWithKey prependFromScan [] grid
    where prependFromScan startPos direction result = (scan grid direction startPos) & tail & map (\pos -> (startPos, pos)) & (++ result)

scan :: Map Pos Direction -> Direction -> Pos -> [Pos]
scan grid Nowhere pos = [pos]
scan grid direction pos
    = if Map.member pos grid
          then pos : scan grid direction (advance direction pos)
          else []

advance :: Direction -> Pos -> Pos
advance Nowhere pos = pos
advance North (Pos x y) = Pos x (y - 1)
advance East (Pos x y) = Pos (x + 1) y
advance South (Pos x y) = Pos x (y + 1)
advance West (Pos x y) = Pos (x - 1) y
advance Northeast pos = (advance North . advance East) pos
advance Southeast pos = (advance South . advance East) pos
advance Southwest pos = (advance South . advance West) pos
advance Northwest pos = (advance North . advance West) pos

initStatus :: [(Pos, Direction, Maybe Value)] -> Status
initStatus datums = Status {
    chainsStartingWith = makeChains datums,
    chainsEndingWith = makeChains datums,
    directions = makeGrid datums,
    possibilities = makePossibilities (makeGrid datums)
    }

  -- chainsStartingWith any pos has length 1
  -- chainsEndingWith any pos has length 1
  -- wait what? that's always true

  -- 1) only one possibility going into a chain
  -- 2) only one possibility coming out of a chain
  -- 3) two chains with values -- they can either link or unlink
  -- 4) two chains, one has a value, the other has length, and following it would go beyond a numeric value in the grid -- unlink that

data Stat = Stat {
    posToChain :: Map Pos Chain,
    possibleLinksByStart :: Map Pos (Seq Pos),
    possibleLinksByEnd :: Map Pos (Seq Pos),
    allPossibleLinks :: Set (Pos, Pos)
    }

solve :: Stat -> Stat
solve stat
    = if Set.size (allPossibleLinks stat) == 1
        then stat
        else foldr execute stat actions
            where actions = solveStep stat

execute :: Action -> Stat -> Stat
execute (Link reason pos1 pos2) stat
    = stat {
        posToChain = posToChain stat & Map.update (\chain -> Just (linkChains chain chain2)) pos1 & Map.delete pos2,
        possibleLinksByStart = possibleLinksByStart stat & insertMaybe (Map.lookup pos2 (possibleLinksByStart stat)) pos1,
        possibleLinksByEnd = possibleLinksByEnd stat & Map.delete pos2,
        allPossibleLinks = allPossibleLinks stat & Set.filter ((/= pos2) . snd) & Set.filter ((/= pos1) . fst) & Set.map (\(a, b) -> if a == pos2 then (pos1, b) else (a, b))
        }
    where chain2 = fromJust (Map.lookup pos2 (posToChain stat))
          linkChains chain1@(Chain mvalue1 poses1) chain2@(Chain mvalue2 poses2)
            = Chain (linkedValue chain1 chain2) (poses1 >< poses2)
          linkedValue (Chain mvalue1 poses1) (Chain mvalue2 poses2)
            = case (mvalue1, mvalue2) of
                (Just v1, Just v2) -> if v1 + Seq.length poses1 == v2 then Just v1 else error "but you can't link these"
                (Just v1, _) -> Just v1
                (_, Just v2) -> Just (v2 - Seq.length poses1)
                otherwise -> Nothing

solveStep :: Stat -> [Action]
solveStep stat = solveStepSingleEntry stat
               ++ solveStepSingleExit stat
               ++ solveStepBothWithValues stat
               ++ solveStepChainConsumesValue stat

solveStepSingleEntry :: Stat -> [Action]
solveStepSingleEntry stat
    = possibleLinksByStart stat
    & Map.filter ((== 1) . Seq.length)
    & Map.foldrWithKey link []
    where link predecessor successors = (Link "single entry" predecessor (Seq.index successors 0) :)

solveStepSingleExit :: Stat -> [Action]
solveStepSingleExit stat
    = possibleLinksByEnd stat
    & Map.filter ((== 1) . Seq.length)
    & Map.foldrWithKey link []
    where link successor predecessors = (Link "single exit" (Seq.index predecessors 0) successor :)

solveStepBothWithValues :: Stat -> [Action]
solveStepBothWithValues stat
    = allPossibleLinks stat
    & Set.toList
    & filter bothHaveValues
    & map makeAction
    where bothHaveValues (Chain mvalue1 _, Chain mvalue2 _) = isJust mvalue1 && isJust mvalue2
          makeAction (chain1, chain2) = if isAscending chain1 chain2
                                          then Link "ascending" chain1 chain2
                                          else Unlink "unascending" chain1 chain2

isAscending :: Chain -> Chain -> Bool
isAscending (Chain mvalue1 _) (Chain mvalue2 _)
    = case (mvalue1, mvalue2) of
        (Just v1, Just v2) -> v1 + 2 == v2
        otherwise -> False

solveStepChainConsumesValue :: Stat -> [Action]
solveStepChainConsumesValue _ = []

main = pure ()
