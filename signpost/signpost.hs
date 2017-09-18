
import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.List
import Data.Maybe

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
    cellPredecessors :: [Pos]
    }

instance Show Cell where
    show cell = "{" ++ show (cellPos cell) ++ " " ++ show (cellDirection cell) ++ " " ++ showValue (cellValue cell) ++ "}"
        where showValue Nothing = "*"
              showValue (Just x) = show x

type Grid = Map Pos Cell
type DirtyGrid = (Set Pos, Grid)

data Direction = Nowhere | North | Northeast | East | Southeast | South | Southwest | West | Northwest

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
            makeCell (pos, direction, value) = Cell { cellPos = pos, cellDirection = direction, cellValue = value, cellSuccessors = [], cellPredecessors = [] }
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
    where actions = solveCell grid cell
          (newDirty, newGrid) = foldl' (flip applyAction) (Set.deleteMin dirtys, grid) actions
          cell = Set.findMin dirtys & (\pos -> Map.lookup pos grid) & fromJust

applyAction :: Action -> DirtyGrid -> DirtyGrid
applyAction (Link pos1 pos2) (dirty1, grid1)
  | not (elem pos1 (cellPredecessors cell2)) = error $ "pos2 can't link back to pos1"
  | not (elem pos2 (cellSuccessors cell1)) = error "pos1 can't link forward to pos2"
  | otherwise = (dirty3, grid3)
  -- (cellSuccessors cell1) == [pos2] && (cellPredecessors cell2) == [pos1] = ([], grid) -- nothing to do -- these cells are already linked
  where cell1 = Map.lookup pos1 grid1 & fromJust
        cell2 = Map.lookup pos2 grid1 & fromJust
        (dirty2, grid2) = if (cellSuccessors cell1) == [pos2]
                              then (dirty1, grid1)
                              else (Set.insert pos1 dirty1, setSuccessors [pos2] pos1 grid1)
        (dirty3, grid3) = if (cellPredecessors cell2) == [pos1]
                              then (dirty2, grid2)
                              else (Set.insert pos2 dirty2, setPredecessors [pos1] pos2 grid2)
        setSuccessors newSuccessors = Map.update (\cell -> Just $ cell { cellSuccessors = newSuccessors })
        setPredecessors newPredecessors = Map.update (\cell -> Just $ cell { cellPredecessors = newPredecessors })

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
            areValuesAscending value1 value2 = maybe2 False (\v1 v2 -> v1 + 1 == v2) (value1, value2)
            lookup pos = Map.lookup pos grid & fromJust
            (realSuccessors, notSuccessors) = successors & partition (areCellsAscending cell)

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
printicate value = show value & replace '[' "[\n" & replace ']' "]\n" & replace ',' ",\n"

applyNTimes :: Int -> (a -> a) -> (a -> a)
applyNTimes n func = foldl1' (.) (replicate n func)

finalised = (sampleDirt, makeGrid sample) & applyNTimes 9 solveStep & snd & Map.toList & map snd & printicate & putStrLn
initialised = makeGrid sample & Map.toList & map snd & printicate & putStrLn
