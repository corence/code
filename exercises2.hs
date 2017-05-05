class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ [] = []
  furry func (x:xs) = func x : furry func xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry func (Just x) = Just (func x)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry a_to_b t_to_a = (\t -> a_to_b (t_to_a t))

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry func (EitherLeft (Left a)) = EitherLeft (Left (func a))
  furry func (EitherLeft (Right t)) = EitherLeft (Right t)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry func (EitherRight (Left t)) = EitherRight (Left t)
  furry func (EitherRight (Right x)) = EitherRight (Right (func x))

class Misty m where
  banana :: (p -> m q) -> m p -> m q
  unicorn :: p -> m p
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (p -> q) -> m p -> m q
  furry' func wrapped_a = banana (\a -> unicorn (func a)) wrapped_a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana func [] = []
  banana func (x:xs) = func x ++ banana func xs
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing = Nothing
  banana func (Just x) = func x
  unicorn x = Just x

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana a_to__t_to_b t_to_a = (\t -> let {a = t_to_a t; t_to_b = a_to__t_to_b a} in t_to_b t)
  unicorn a = \_ -> a

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana a_to_el_b (EitherLeft (Left a)) = a_to_el_b a
  banana _ (EitherLeft (Right t)) = EitherLeft (Right t)
  unicorn a = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left t)) = EitherRight (Left t)
  banana a_to_er_b (EitherRight (Right a)) = a_to_er_b a
  unicorn a = EitherRight (Right a)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

call :: (a -> b) -> a -> b
call func a = func a

call2 :: a -> (a -> b) -> b
call2 a func = func a

semiapple :: (Misty m) => a -> m (a -> b) -> m b
semiapple a wrapped_func = furry' (call2 a) wrapped_func

semiapple2 :: (Misty m) => m (a -> b) -> a -> m b
semiapple2 wrapped_func a = furry' (call2 a) wrapped_func

-- Exercise 13
-- Relative Difficulty: 6
  -- banana :: (p -> m q) -> m p -> m q
  -- banana :: (p -> m b) -> m p -> m b
  
  -- furry' :: (p -> q) -> m p -> m q
  -- furry' :: (p -> q) -> m p -> m q
  -- furry' :: (a -> b) -> m a -> m b
  -- furry' :: ((a -> b) -> m b) -> m (a -> b) -> m m b
  
  -- furry' :: (a -> b) -> m a -> m b
  -- furry' :: ((a -> b) -> q) -> m (a -> b) -> m q
  
  -- furry' :: (m a -> b) -> m a -> m b
  
  -- furry' :: (a -> m b) -> m a -> m m b
  -- furry' :: ((a -> b) -> b) -> m (a -> b) -> m b
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple wrapped_a wrapped_func = jellybean (furry' (semiapple2 wrapped_func) wrapped_a)

-- Exercise 14
-- Relative Difficulty: 6
  -- banana :: (p -> m q) -> m p -> m q
  -- banana :: (p -> m [b]) -> m p -> m [b]
  
  -- banana :: (p -> h q) -> h p -> h q
  
  -- furry' :: (p -> q) -> m p -> m q
  -- furry' :: (a -> q) -> [a] -> [q]
  -- furry' :: (p -> [b]) -> m p -> m [b]
  
  -- furry' :: (p -> q) -> h p -> h q
  -- furry' :: (p -> [b]) -> m p -> m [b]
  
  -- furry' :: ([a] -> [b]) -> h [a] -> h [b]
  
  -- jellybean :: m (m p) -> m p
  -- jellybean :: m (m b) -> m b
  
  -- apple :: h p -> h (p -> q) -> h q
  -- apple :: m p -> m (p -> [b]) -> m [b]

  -- from the end:
  -- apple :: m p -> m (p -> [b]) -> m [b]
  -- furry' :: (p -> [b]) -> m p -> m [b]
  -- banana :: (p -> m [b]) -> m p -> m [b]
  
  -- furry' :: (p -> [b]) -> m p -> m [b]
  -- furry' :: (p -> [b]) -> m p -> m [b]

  -- apple :: [p] -> [p -> b] -> [b]
  -- furry' :: (p -> b) -> [p] -> [b]
  -- banana :: (p -> [b]) -> [p] -> [b]
  
  -- apple :: m p -> m (p -> [b]) -> m [b]
  -- furry' :: (p -> [b]) -> m p -> m [b]
  -- banana :: (p -> m [b]) -> m p -> m [b]
  
  -- from the start:
  -- banana :: (a -> [b]) -> [a] -> [b]
  -- apple :: [a] -> [a -> q] -> [q]
  -- furry' :: (a -> m b) -> [a] -> [m b]
  
  -- clever jellybean:
  -- jellybean :: h (h p) -> h p
  -- jellybean :: [[b]] -> [b]
  -- jellybean :: m (m b) -> m b

  -- clever sausage:
  -- sausage :: [m b] -> m [b]
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy la a_to_mb = sausage (furry' a_to_mb la)

moppy2 :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy2 [] a_to_mb = unicorn []
moppy2 (a:as) a_to_mb = banana2 (:) (a_to_mb a) (moppy2 as a_to_mb)

moppy3 :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy3 [] a_to_mb = unicorn []
moppy3 (a:as) a_to_mb = apple (moppy3 as a_to_mb) (furry' (:) (a_to_mb a))

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
  -- apple :: h p -> h (p -> q) -> h q
  -- furry' :: (p -> q) -> m p -> m q
  -- banana :: (p -> m q) -> m p -> m q
  
  -- apple :: m p -> m (p -> [a]) -> m [a]
  -- furry' :: (p -> [a]) -> m p -> m [a]
  -- banana :: (p -> m [a]) -> m p -> m [a]
  
  -- apple :: [p] -> [p -> a] -> [a]
  -- furry' :: (p -> q) -> [p] -> [a]
  -- banana :: (p -> [a]) -> [p] -> [a]

  -- from start:
  -- apple :: p -> h (p -> q) -> h q
  -- furry' :: (p -> q) -> m p -> m q
  -- banana :: (p -> m q) -> m p -> m q
  
sausage :: (Misty m) => [m a] -> m [a]
sausage list = moppy list id

sausage2 :: (Misty m) => [m a] -> m [a]
sausage2 [] = unicorn []
sausage2 (ma:mas) = banana2 (:) ma (sausage2 mas)

sausage3 :: (Misty m) => [m a] -> m [a]
sausage3 mas = foldr (\ma result -> banana2 (:) ma result) (unicorn []) mas

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
-- apple :: (Misty m) => m a -> m (a -> b) -> m b
-- furry' :: (p -> q) -> m p -> m q
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 func wrapped_a wrapped_b = apple wrapped_b (furry' func wrapped_a)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 func w_a w_b w_c = apple w_c (banana2 func w_a w_b)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 func w_a w_b w_c w_d = apple w_d (banana3 func w_a w_b w_c)

newtype StateChange stateType outcomeType = StateChange {
  transition :: (stateType -> (stateType, outcomeType))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (StateChange stateType) where
  -- make a function that takes oldState and returns the newState and the modified outcome
  furry func stateChange = StateChange { transition = newTransition }
    where newTransition oldState = (\(newState, newOutcome) -> (newState, func newOutcome)) ((transition stateChange) oldState)
  --furry :: (a -> b) -> f a -> f b
  --furry :: (outcome1 -> outcome2) -> f outcome1 -> f outcome2
  --furry :: (outcome1 -> outcome2) -> StateChange stateType outcome1 -> StateChange stateType outcome2

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (StateChange state) where
  -- banana :: (p -> m q) -> m p -> m q
  banana func firstStateChange = StateChange { transition = mergedTransition }
      where mergedTransition state0 = let (state1, outcome1) = (transition firstStateChange) state0
                                          secondStateChange = func outcome1
                                      in (transition secondStateChange) state1
  unicorn outcome = StateChange { transition = (\state -> (state, outcome)) }

main = return ()
