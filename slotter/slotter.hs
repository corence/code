
candidates = [1,2,3]

extendAll :: [a] -> [[a]] -> [[a]]
extendAll elements bases = concat $ map extend bases
    where extend base = map (\e -> e : base) elements

allCreations :: [a] -> [[a]] -> [[a]]
allCreations elements bases = newBases ++ allCreations elements newBases
    where newBases = extendAll elements bases

main = do
    putStrLn $ show $ take 2 (congregate candidates [])


infinity x = x ++ infinity x

infinitude :: Num x => [x] -> [x]
infinitude x = sirius x ++ infinitude (sirius x)

q = infinitude [4]

sirius :: Num x => [x] -> [x]
sirius = map (+ 2)

-- input: [1,2,3]
-- output: ["1","2","3","11","12","13","21","22","23",...]
-- extensions = ["11", "12", "13", "21", "22", "23"]
congregate :: Show a => [a] -> [String] -> [String]
congregate candidates bases = extensions candidates bases ++ congregate candidates (extensions candidates bases)

extensions :: Show a => [a] -> [String] -> [String]
extensions candidates bases = foldr (\base results -> extend candidates base results) [] bases

extend :: Show a => [a] -> String -> [String] -> [String]
extend candidates base results = foldr (\candidate output -> (show candidate ++ base) : output) results candidates
 
dfs :: Show a => ([a] -> Bool) -> [a] -> [a] -> [[a]]
dfs tester candidates branch = map followBuild candidates
    where followBuild candidate = if tester built then built ++ dfs tester candidates built else dfs tester candidates built
            where built = candidate : branch
