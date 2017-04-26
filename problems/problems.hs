
data Problem = NullPointerProblem | IndexOutOfBoundsProblem Int | IOProblem | DietaryProblem Animal Animal deriving Show
data Problematic a = Good a | Bad Problem

tryCatch :: Problematic a -> (Problem -> a) -> a -- tryBlock, catchBlock, result
tryCatch tryBlock catchBlock
  = case tryBlock of
        Good result -> result
        Bad problem -> catchBlock problem

data Animal = Jaguar | Giraffe | Echidna | Ant deriving Show

-- one of the animals will eat the other! If not, we have a DietaryProblem
eat :: Animal -> Animal -> Problematic Animal
eat Jaguar Giraffe = Good Jaguar
eat Giraffe Jaguar = Good Jaguar
eat Echidna Ant = Good Echidna
eat Ant Echidna = Good Echidna
eat animal1 animal2 = Bad (DietaryProblem animal1 animal2)

-- the 3 animals are going to eat each other in conga-line order
-- If this fails (because the animals are the wrong types), print a warning message and return Nothing
tryEating :: Animal -> Animal -> Animal -> (String, Maybe Animal)
tryEating animal1 animal2 animal3
    = tryCatch tryBlock catchBlock
        where tryBlock = (
                      case eat animal1 animal2 of
                          Bad problem -> Bad problem
                          Good survivor1 -> case eat survivor1 animal3 of
                                              Bad problem -> Bad problem
                                              Good survivor2 -> Good ("dinner finished!", Just survivor2)
                      )
              catchBlock (DietaryProblem animal1 animal2) = (show animal1 ++ " tried to eat " ++ show animal2 ++ " but it didn't taste right", Nothing)
              catchBlock problem = ("something went wrong: " ++ show problem, Nothing)
        
main = return ()
