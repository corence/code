
import Control.Monad.State

type Stack = [Int]  
  
{-
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  

newtype State s a = State { runState :: s -> (a,s) }  

unit :: a -> State s a
unit x = State $ \s -> (x,s)  

bind :: (State s a) -> (a -> State s b) -> (State s b)
bind :: (State s Int) -> (Int -> State s Float) -> (State s Float)
bind (State first_transformer) transform_generator = State $ \initial_state -> let (a, second_state) = first_transformer initial_state
                                                                                   (State generated_transformer) = transform_generator a  
                                                                                   in generated_transformer second_state  
-}                                    

pop :: State Stack Int
pop = state (\(x:xs) -> (x,xs))
  
push :: Int -> State Stack ()
push value = state (\(xs) -> ((),value:xs))


main = do
    return ()

stackManip3 :: State Stack Int  
stackManip3 = push 3 >> push 4 >> push 5 >> pop
    
stackManip2 :: State Stack Int  
stackManip2 = do  
    push 3  
    push 4
    push 5
    pop
    
stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop  
    
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8     
            
stackStuff2 :: State Stack ()  
stackStuff2 = pop >>= (\z -> cond ((==) 5 z)
                                 (push 5)
                                 (push 3 >> push 8))

cond :: Bool -> a -> a -> a
cond True a _ = a
cond False _ a = a
