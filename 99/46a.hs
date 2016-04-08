
runProblem :: (Eq t, Show t) => String -> t -> t -> IO ()
runProblem pid expected actual
  | expected == actual = return ()
  | otherwise = putStrLn $ pid ++ ": [" ++ show expected ++ "] != [" ++ show actual ++ "]"
  
main = do
  runProblem "50a" [('a', 45), ('d', 16), ('b', 13), ('c', 12), ('e', 9), ('f', 5)] $ sort50 (\(_, frequency) -> -frequency) [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  runProblem "50b" [(HuffLeaf 'f' 5),(HuffLeaf 'e' 9),(HuffLeaf 'c' 12),(HuffLeaf 'd' 16)] $ sort50 frequency $ map freqToHuff [('c',12),('d',16),('e',9),('f',5)]
  runProblem "50c" [(HuffNode (HuffLeaf 'f' 5) (HuffLeaf 'e' 9) 14)] $ huffEncodeStep50 $ sort50 frequency $ map freqToHuff [('e',9),('f',5)]
  runProblem "50d" [(HuffNode (HuffLeaf 'f' 5) (HuffLeaf 'e' 9) 14), (HuffLeaf 'd' 16)] $ huffEncodeStep50 $ sort50 frequency $ map freqToHuff [('d',16),('e',9),('f',5)]
  runProblem "50e" [(HuffNode (HuffNode (HuffLeaf 'f' 5) (HuffLeaf 'e' 9) 14) (HuffLeaf 'd' 16) 30)] $ huffEncodeStep50 . huffEncodeStep50 $ sort50 frequency $ map freqToHuff [('d',16),('e',9),('f',5)]
  putStrLn $ huffPrints [('a',45)]
  putStrLn $ huffPrints [('a',45),('b',13)]
  putStrLn $ huffPrints [('a',45),('b',13),('d',16)]
  putStrLn $ huffPrints [('a',45),('b',13),('c',12),('d',16)]
  putStrLn $ huffPrints [('a',45),('b',13),('c',12),('d',16),('e',9)]
  let huffTree = (huffEncode50 $ sort50 frequency $ map freqToHuff [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]) in do
      putStrLn $ printTreeToSExpressions huffTree
      putStrLn $ stringLines $ printTreeIndented 0 "  " huffTree
      putStrLn $ stringLines $ printTree huffTree
      runProblem "50f" (Just [East, East, East]) $ valueToHuffCode huffTree 'd'
      runProblem "50g" (Just [East, West, West]) $ valueToHuffCode huffTree 'c'
      runProblem "50h" (Just [East, East, West, West]) $ valueToHuffCode huffTree 'f'
      runProblem "50i" (Just [East, West, East]) $ valueToHuffCode huffTree 'b'
      runProblem "50j" (Just 'f') $ huffCodeToValue huffTree [East, East, West, West]
      runProblem "50k" Nothing $ huffCodeToValue huffTree [West, East, East, West, West]
      runProblem "50l" Nothing $ valueToHuffCode huffTree 'q'

type Freq a = (a, Int)

data Direction = West | East deriving (Show, Eq)
type HuffCode = [Direction]

data HuffTree a = HuffNode (HuffTree a) (HuffTree a) Int | HuffLeaf a Int
    deriving (Eq, Show)

huffPrints :: (Show a) => [Freq a] -> String
huffPrints freqs = let huffTree = huffEncode50 $ sort50 frequency $ map freqToHuff freqs in
  "---------------\n" ++ (printTreeToSExpressions huffTree) ++ "\n" ++ (stringLines $ printTreeIndented 0 "  " huffTree) ++ (stringLines $ printTree huffTree) ++ "---------------"

printTreeToSExpressions :: (Show a) => HuffTree a -> String
printTreeToSExpressions (HuffLeaf value frequency) = "(" ++ show frequency ++ ":" ++ show value ++ ")"
printTreeToSExpressions (HuffNode leftTree rightTree frequency) = "(" ++ show frequency ++ ":" ++ printTreeToSExpressions leftTree ++ " " ++ printTreeToSExpressions rightTree ++ ")"

printTreeIndented :: (Show a) => Int -> String -> HuffTree a -> [String]
printTreeIndented indent marker (HuffLeaf value frequency) = [makeSpace indent ++ marker ++ show frequency ++ ":" ++ show value]
printTreeIndented indent marker (HuffNode leftTree rightTree frequency) = [makeSpace indent ++ marker ++ show frequency ++ ":"] ++ printTreeIndented (indent + 2) "< " leftTree ++ printTreeIndented (indent + 2) "> " rightTree

printTree :: (Show a) => HuffTree a -> [String]
printTree (HuffLeaf value frequency) = [show frequency ++ ":" ++ show value]
printTree (HuffNode leftTree rightTree frequency) = header : foundations
  where foundations = mixStrings (printTree leftTree) separator (printTree rightTree)
        header = centerString (length $ head foundations) myNode
        myNode = show frequency ++ ":*"
        separator = makeSpace (2 + length myNode)

stringLines :: [String] -> String
stringLines [] = ""
stringLines (x:xs) = x ++ "\n" ++ stringLines xs

mergeStrings :: [String] -> String -> [String] -> [String]
mergeStrings [] _ [] = []
mergeStrings (l:ls) _ [] = error $ "mergeStrings: left strings are too long" ++ (show (l:ls))
mergeStrings [] _ (r:rs) = error $ "mergeStrings: right strings are too long: " ++ (show (r:rs))
mergeStrings (l:ls) separator (r:rs) = ((centerString (length r) l) ++ separator ++ (centerString (length l) r)) : mergeStrings ls separator rs

mixStrings :: [String] -> String -> [String] -> [String]
mixStrings ls separator rs
  | (length ls > length rs) = mixStrings ls separator (rs ++ [makeSpace $ length $ head rs])
  | (length ls < length rs) = mixStrings (ls ++ [makeSpace $ length $ head ls]) separator rs
  | otherwise = mergeStrings ls separator rs

centerString :: Int -> String -> String
centerString width s = leftPadding ++ s ++ rightPadding
  where leftPadding = makeSpace ((width - length s) `div` 2)
        rightPadding = makeSpace ((width - length s + 1) `div` 2)
        
makeArray :: Int -> a -> [a]
makeArray 0 _ = []
makeArray length element = element : makeArray (length-1) element

makeSpace :: Int -> String
makeSpace n
  | n >= 0 = makeArray n '.'
  | otherwise = ""

longest :: [[a]] -> [a]
longest [] = error "the longest array is no array. zen?"
longest [single] = single
longest (x:xs)
  | (length x) >= (length contender) = x
  | otherwise = contender
  where contender = longest xs

freqToHuff :: Freq a -> HuffTree a
freqToHuff (value, frequency) = HuffLeaf value frequency

-- exhaustive search :(
valueToHuffCode :: Eq a => HuffTree a -> a -> Maybe HuffCode
valueToHuffCode (HuffLeaf hValue hFrequency) value
  | hValue == value = Just []
  | otherwise = Nothing
valueToHuffCode (HuffNode leftTree rightTree _) value = case (valueToHuffCode leftTree value) of
  Just huffLeft -> Just (West : huffLeft)
  Nothing -> case (valueToHuffCode rightTree value) of
    Just huffRight -> Just (East : huffRight)
    Nothing -> Nothing

-- O(n) where n is the length of the huff code :)
huffCodeToValue :: Eq a => HuffTree a -> HuffCode -> Maybe a
huffCodeToValue (HuffLeaf hValue _) [] = Just hValue
huffCodeToValue (HuffLeaf _ _) _ = Nothing
huffCodeToValue (HuffNode leftTree _ _) (West:codes) = huffCodeToValue leftTree codes
huffCodeToValue (HuffNode _ rightTree _) (East:codes) = huffCodeToValue rightTree codes

huffEncode50 :: [HuffTree a] -> HuffTree a
huffEncode50 [] = error "needs more huffs imo"
huffEncode50 [single] = single
huffEncode50 huffs = huffEncode50 (huffEncodeStep50 huffs)

huffEncodeStep50 :: [HuffTree a] -> [HuffTree a]
huffEncodeStep50 [] = []
huffEncodeStep50 [single] = [single]
huffEncodeStep50 (x:y:xs) = sortedInsert frequency (HuffNode x y (frequency x + frequency y)) xs

frequency :: HuffTree a -> Int
frequency (HuffNode _ _ result) = result
frequency (HuffLeaf _ result) = result

sortedInsert :: Ord comp => (t -> comp) -> t -> [t] -> [t]
sortedInsert _ element [] = [element]
sortedInsert pred element (x:xs)
  | pred element <= pred x = element : x : xs
  | otherwise = x : (sortedInsert pred element xs)

sort50 :: Ord comp => (t -> comp) -> [t] -> [t]
sort50 pred list = merge50 pred (explode50 list)

explode50 :: [t] -> [[t]]
explode50 [] = []
explode50 (x:xs) = [x] : explode50 xs

merge50 :: Ord comp => (t -> comp) -> [[t]] -> [t]
merge50 pred [] = []
merge50 pred (x:[]) = x
merge50 pred lists = merge50 pred (mergeStep50 pred lists)

mergeStep50 :: Ord comp => (t -> comp) -> [[t]] -> [[t]]
mergeStep50 pred [] = []
mergeStep50 pred (x:[]) = [x]
mergeStep50 pred (x:y:xs) = (mergeTwo50 pred x y) : mergeStep50 pred xs

mergeTwo50 :: Ord comp => (t -> comp) -> [t] -> [t] -> [t]
mergeTwo50 pred [] ys = ys
mergeTwo50 pred xs [] = xs
mergeTwo50 pred (x:xs) (y:ys) =
  if pred x < pred y
      then x : mergeTwo50 pred xs (y:ys)
      else y : mergeTwo50 pred (x:xs) ys
