
newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a

instance Monad (State s) where
    return :: a -> State s a
    
    return x = state ( \ st -> (x, st) )
    
    (>>=) :: State s a -> (a -> State s b) -> State s b
    
    pr >>= k = state $ \ st ->
        let (x, st') = runState pr st -- Running the first processor on st.
        in runState (k x) st'       -- Running the second processor on st'.


main = do
  putStrLn "stating"
