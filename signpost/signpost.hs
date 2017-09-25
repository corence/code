
import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.List
import Data.Maybe
import Control.Applicative
import Debug.Trace

(&) = flip ($)

for :: b -> [a] -> (b -> a -> b) -> b
for initValue series function = foldl' function initValue series

maybe2 :: c -> (a -> b -> c) -> (Maybe a, Maybe b) -> c
maybe2 fallback _ (Nothing, _) = fallback
maybe2 fallback _ (_, Nothing) = fallback
maybe2 _ func (Just a, Just b) = func a b

data Pos = Pos Int Int deriving (Eq, Ord, Show)

type Value = Int
data Cell = Cell {
    cellPos :: Pos,
    cellDirection :: Direction,
    cellValue :: Maybe Value,
    cellSuccessors :: [Pos],
    cellPredecessors :: [Pos],
    cellConnection :: Maybe Pos
    } deriving (Eq)

instance Show Cell where
    show cell
        = "{"
        ++ showCellThing cellPos
        ++ " "
        ++ showCellThing cellDirection
        ++ " "
        ++ showCellMaybe cellValue
        ++ " <"
        ++ showCellThing (length . cellPredecessors)
        ++ ":"
        ++ showCellThing (length . cellSuccessors)
        ++ "> -> "
        ++ showCellMaybe cellConnection
        ++ "}"
        where showCellMaybe func = maybe "*" show (func cell)
              showCellThing func = show (func cell)

type Grid = Map Pos Cell
type DirtyGrid = (Set Pos, Grid)

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

makeGrid :: [(Pos, Direction, Maybe Value)] -> Grid
makeGrid datums = makeInitialGrid & linkForward & linkBackward
    where   makeInitialGrid = foldl' (\grid datum@(pos, direction, value) -> Map.insert pos (makeCell datum) grid) Map.empty datums
            makeCell (pos, direction, value) = Cell { cellPos = pos, cellDirection = direction, cellValue = value, cellSuccessors = [], cellPredecessors = [], cellConnection = Nothing }
            linkForward grid = foldl' linkCellForward grid (Map.toList grid & map snd)
                where   linkCellForward grid cell = Map.insert (cellPos cell) (newCell cell) grid
                        newCell cell = cell { cellSuccessors = scan grid (cellDirection cell) (cellPos cell) & tail }
            linkBackward grid = foldl' linkCellBackward grid (Map.toList grid & map snd)
                where   linkCellBackward grid cell = Map.insert (cellPos cell) (newCell cell) grid
                        newCell cell = cell { cellPredecessors = (predecessors cell) }
                        predecessors cell = Map.toList grid & map fst & filter (\p -> elem p (cellSuccessors cell))

scan :: Grid -> Direction -> Pos -> [Pos]
scan grid Nowhere pos = [pos]
scan grid direction pos
    = if Map.member pos grid
          then pos : scan grid direction (advance direction pos)
          else []

advance :: Direction -> Pos -> Pos
advance Nowhere pos = pos
advance North (Pos x y) = Pos x (y + 1)
advance East (Pos x y) = Pos (x + 1) y
advance South (Pos x y) = Pos x (y - 1)
advance West (Pos x y) = Pos (x - 1) y
advance Northeast pos = (advance North . advance East) pos
advance Southeast pos = (advance South . advance East) pos
advance Southwest pos = (advance South . advance West) pos
advance Northwest pos = (advance North . advance West) pos

sample :: [(Pos, Direction, Maybe Value)]
sample = [
    (Pos 0 0, East, Just 1),
    (Pos 0 1, Southwest, Nothing),
    (Pos 0 2, Southwest, Nothing),
    (Pos 1 0, East, Nothing),
    (Pos 1 1, North, Nothing),
    (Pos 1 2, Southwest, Nothing),
    (Pos 2 0, Northeast, Nothing),
    (Pos 2 1, East, Nothing),
    (Pos 2 2, Nowhere, Just 9)
    ]

sampleDirt :: Set Pos
sampleDirt = map (\(a, _, _) -> a) sample & Set.fromList

data Action = Link Pos Pos | Unlink Pos Pos

solve :: DirtyGrid -> Grid
solve (dirtys, grid)
    = if Set.null dirtys
          then grid
          else solve (solveStep (dirtys, grid))

solveStep :: DirtyGrid -> DirtyGrid
solveStep (dirtys, grid)
  | Set.null dirtys = (dirtys, grid)
  | otherwise = (Set.deleteMin dirtys, newGrid)
    where actions = solveCell2 grid cell
          (newDirty, newGrid) = foldr applyAction (Set.deleteMin dirtys, grid) actions
          cell = Set.findMin dirtys & (\pos -> Map.lookup pos grid) & fromJust

areConnected :: Grid -> Pos -> Pos -> Bool
areConnected grid pos1 pos2
  = (elem pos1 (cellPredecessors cell2)) && (elem pos2 (cellSuccessors cell1))
  where cell1 = Map.lookup pos1 grid & fromJust
        cell2 = Map.lookup pos2 grid & fromJust

applyAction :: Action -> DirtyGrid -> DirtyGrid
applyAction (Link pos1 pos2) (dirty1, grid1) = trace ("a " ++ show cell1 ++ " -> " ++ show newCell1 ++ ", " ++ show cell2 ++ " -> " ++ show newCell2) $ (dirty3, grid3)
  where cell1 = Map.lookup pos1 grid1 & fromJust
        cell2 = Map.lookup pos2 grid1 & fromJust
        dirty2 = if cell1 == newCell1
                     then dirty1
                     else foldr Set.insert dirty1 (relatedPoses cell1)
        dirty3 = if cell2 == newCell2
                     then dirty2
                     else foldr Set.insert dirty2 (relatedPoses cell2)
        grid2 = Map.insert pos1 newCell1 grid1
        grid3 = Map.insert pos2 newCell2 grid2
        (newCell1, newCell2) = link (cell1, cell2)
        relatedPoses cell = cellPos cell : cellPredecessors cell ++ cellSuccessors cell

applyAction (Unlink pos1 pos2) (dirty1, grid1) = trace ("b " ++ show cell1 ++ " -> " ++ show newCell1 ++ ", " ++ show cell2 ++ " -> " ++ show newCell2) $ (dirty3, grid3)
  where cell1 = Map.lookup pos1 grid1 & fromJust
        cell2 = Map.lookup pos2 grid1 & fromJust
        newCell1 = cell1 { cellSuccessors = delete pos2 (cellSuccessors cell1) }
        newCell2 = cell2 { cellPredecessors = delete pos1 (cellPredecessors cell2) }
        dirty2 = if cell1 == newCell1
                     then dirty1
                     else foldr Set.insert dirty1 (relatedPoses cell1)
        dirty3 = if cell2 == newCell2
                     then dirty2
                     else foldr Set.insert dirty2 (relatedPoses cell2)
        grid2 = Map.insert pos1 newCell1 grid1
        grid3 = Map.insert pos2 newCell2 grid2
        relatedPoses cell = cellPos cell : cellPredecessors cell ++ cellSuccessors cell

link :: (Cell, Cell) -> (Cell, Cell)
link (cell1, cell2) = (newCell1, newCell2)
    where newCell1 = cell1 { cellSuccessors = [], cellConnection = Just (cellPos cell2) } & assignValue (subtract 1) (cellValue cell2)
          newCell2 = cell2 { cellPredecessors = [] } & assignValue (+1) (cellValue cell1)
          assignValue _ Nothing cell = cell
          assignValue func (Just value) cell
            | cellValue cell == (Just value) = cell
            | cellValue cell == Nothing = cell { cellValue = Just (func value) }
            | otherwise = error "incompatible values lol!"

solveCell :: Grid -> Cell -> [Action]
solveCell grid cell = actionsFromSoloSuccessors ++ actionsFromSuccessorValues (cellValue cell) ++ actionsFromSoloPredecessors ++ actionsFromPredecessorValues (cellValue cell)
    where actionsFromSoloSuccessors = soloSuccessors & map (\s -> Link (cellPos cell) (cellPos s))
          soloSuccessors = cellSuccessors cell & map lookup & filter (\s -> cellPredecessors s == [cellPos cell])
          actionsFromSoloPredecessors = soloPredecessors & map (\p -> Link (cellPos p) (cellPos cell))
          soloPredecessors = cellPredecessors cell & map lookup & filter (\p -> cellSuccessors p == [cellPos cell])
          actionsFromSuccessorValues Nothing = []
          actionsFromSuccessorValues (Just v1) = successorsWithValues & partition ((== v1 + 1) . fromJust . cellValue) & (\(linky, unlinky) -> map (\p -> Link (cellPos cell) (cellPos p)) linky ++ map (\p -> Unlink (cellPos cell) (cellPos p)) unlinky)
          successorsWithValues = cellSuccessors cell & map lookup & filter (isJust . cellValue)
          actionsFromPredecessorValues Nothing = []
          actionsFromPredecessorValues (Just v2) = predecessorsWithValues & partition ((== v2 - 1) . fromJust . cellValue) & (\(linky, unlinky) -> map (\s -> Link (cellPos cell) (cellPos s)) linky ++ map (\s -> Unlink (cellPos cell) (cellPos s)) unlinky)
          predecessorsWithValues = cellPredecessors cell & map lookup & filter (isJust . cellValue)
          lookup pos = Map.lookup pos grid & fromJust

solveCell2 :: Grid -> Cell -> [Action]
solveCell2 grid cell
    = actionsFromSoloSuccessors
    ++ actionsFromSoloPredecessors
    ++ actionsFromSuccessorValues grid cell
    where actionsFromSoloSuccessors = if 1 == length (cellSuccessors cell) then [Link (cellPos cell) (head (cellSuccessors cell))] else []
          actionsFromSoloPredecessors = if 1 == length (cellPredecessors cell) then [Link (head (cellPredecessors cell)) (cellPos cell)] else []

actionsFromSuccessorValues :: Grid -> Cell -> [Action]
actionsFromSuccessorValues grid cell
  = map (\realSuccessor -> Link (cellPos cell) (cellPos realSuccessor)) realSuccessors
  ++ map (\notSuccessor -> Unlink (cellPos cell) (cellPos notSuccessor)) notSuccessors
      where successors = cellSuccessors cell & map lookup
            areCellsAscending cell1 cell2 = areValuesAscending (cellValue cell1) (cellValue cell2)
            areValuesAscending value1 value2 = maybe2 False areAscending (value1, value2)
            areValuesAscending2 mvalue1 mvalue2 = liftA2 areAscending mvalue1 mvalue2 & (== (Just True))
            lookup pos = Map.lookup pos grid & fromJust
            (realSuccessors, notSuccessors) = successors & partition (areCellsAscending cell)
            areAscending value1 value2 = value1 + 1 == value2

areCellsAscendingInValue :: [Cell] -> Bool
areCellsAscendingInValue [] = True
areCellsAscendingInValue (x:[]) = True
areCellsAscendingInValue (cell1:cell2:cells) = maybe2 False (\v1 v2 -> v1 + 1 == v2) (maybeValue1, maybeValue2)
    where maybeValue1 = cellValue cell1
          maybeValue2 = cellValue cell2

main = pure ()

indent :: Int -> String
indent n = repeat ' ' & take (n * 2)

replace :: Eq a => a -> [a] -> [a] -> [a]
replace _ _ [] = []
replace oldValue newValues (x:xs)
  = if x == oldValue
        then newValues ++ next
        else x : next
            where next = replace oldValue newValues xs

printicate :: (Show a) => a -> String
printicate value = show value & replace '[' "[\n" & replace ']' "\n]\n" & replace ',' ",\n"

applyNTimes :: Int -> (a -> a) -> (a -> a)
applyNTimes n func = foldl1' (.) (replicate n func)

finalised = (sampleDirt, makeGrid sample) & applyNTimes 9 solveStep & snd & printGrid
initialised = makeGrid sample & printGrid
printGrid grid = Map.toList grid & map snd & printicate & putStrLn
