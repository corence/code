class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry a_to_b [] = []
  furry a_to_b (x:xs) = a_to_b x : furry a_to_b xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry a_to_b (Just a) = Just (a_to_b a)
  furry a_to_b Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry a_to_b t_to_a = (\arg -> a_to_b (t_to_a arg))

newtype EitherLeft q p = EitherLeft (Either p q) deriving Show
newtype EitherRight p q = EitherRight (Either p q) deriving Show

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry a_to_b (EitherLeft (Left x)) = EitherLeft (Left (a_to_b x))
  furry a_to_b (EitherLeft (Right x)) = EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry a_to_b (EitherRight (Left x)) = EitherRight (Left x)
  furry a_to_b (EitherRight (Right x)) = EitherRight (Right (a_to_b x))

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' a_to_b wrapped_a = banana (unicorn . a_to_b) wrapped_a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  --banana a_to_wrapped_b wrapped_a = concat (map a_to_wrapped_b wrapped_a)
  banana a_to_list_b [] = []
  banana a_to_list_b (a:as) = (a_to_list_b a) ++ (banana a_to_list_b as)
  unicorn value = [value]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana a_to_maybe_b (Just a) = a_to_maybe_b a
  banana a_to_maybe_b Nothing = Nothing
  unicorn a = Just a

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a -> (t -> b)) -> (t -> a) -> (t -> b) 
  banana a_to__t_to_b t_to_a = (\t -> (a_to__t_to_b (t_to_a t) t)) -- this is probably totally wrong -- we've applied t to 2 different functions :/
  -- unicorn :: a -> (t -> a)
  unicorn a = (\t -> a) -- this throws away the argument. that doesn't seem good at all

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana a_to_el_b (EitherLeft (Left a)) = a_to_el_b a
  banana a_to_el_b (EitherLeft (Right t)) = EitherLeft (Right t)
  unicorn a = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  --banana :: (a -> m b) -> m a -> m b
  banana a_to_er_b (EitherRight (Right a)) = a_to_er_b a
  banana a_to_er_b (EitherRight (Left t)) = EitherRight (Left t)
  unicorn a = EitherRight (Right a)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean m_m_a = banana id m_m_a
-- jellybean (m (m a)) = m a
-- bind :: (a -> m b) -> m a -> m b
-- fmap :: (a -> b) -> m a -> m b
-- unit :: a -> m a

jellybean1 (Just (Just a)) = Just a
jellybean1 (Just Nothing) = Nothing
jellybean1 Nothing = Nothing

jellybeanA :: Misty m => m (m a) -> m a
--jellybeanA uncertain_thing = furry (\m_a -> furry (\a -> 
--jellybeanA m_m_a = banana (\m_a -> banana (\a -> 
jellybeanA m_m_a = banana id m_m_a

jellybean2 [[]] = []
jellybean2 ([]:lists) = jellybean lists
jellybean2 ((e:es):lists) = e : jellybean (es:lists)

-- [[1,2],[3,4]] -> [1,2,3,4]
--jellybeanB :: [[a]] -> [a]
--jellybeanB l_l_a = banana 

--jb_result :: [a] -> 

jellybean3 :: (t -> t -> a) -> (t -> a)
jellybean3 doublefunc = (\t -> (doublefunc t) t)

jellybean4 (EitherRight (Right (EitherRight (Right a)))) = EitherRight (Right a)
jellybean4 (EitherRight (Right (EitherRight (Left a)))) = EitherRight (Left a)
jellybean4 (EitherRight (Left t)) = EitherRight (Left t)

--jellybean12 :: Maybe (Maybe a) -> Maybe a
--jellybean12 param = 

testcase1 :: (Int -> Int)
testcase1 = jellybean3 (+)

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple wrapped_a wrapped_func = jellybean $ furry' (\func -> furry' (\a -> func a) wrapped_a) wrapped_func

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
--  furry :: (a -> b) -> f a -> f b
--  banana :: (a -> m b) -> m a -> m b
--  unicorn :: a -> m a
--  jellybean :: (Misty m) => m (m a) -> m a
--  apple :: (Misty m) => m a -> m (a -> b) -> m b
moppy list_of_a a_to_mb = error "todo"

--moppy1 :: [a] -> (a -> Maybe b) -> Maybe [b]
--moppy1 list_of_a a_to_mb = furry () list_of_a

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

sausage1 :: [Maybe a] -> Maybe [a]
sausage1 [] = Nothing
sausage1 (Nothing : as) = sausage1 as
sausage1 (Just a : as) = case sausage1 as of
                             Just list -> Just (a : list)
                             Nothing -> Just [a]

sausage2 :: [t -> a] -> (t -> [a])
sausage2 funcs = (\t -> furry (\func -> func t) funcs)

sausage3 :: [EitherRight t a] -> EitherRight t [a]
sausage3 [] = EitherRight (Right [])
sausage3 ((EitherRight (Left result)) : _) = EitherRight (Left result)
sausage3 ((EitherRight (Right value)) : es) = case (sausage3 es) of
                                                  EitherRight (Left result) -> EitherRight (Left result)
                                                  EitherRight (Right values) -> EitherRight (Right (value : values))

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 func w_a w_b = apple w_b (furry' func w_a)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
--banana :: (a -> m b) -> m a -> m b
--banana2 :: (Misty m) => (a -> (b -> c)) -> m a -> (m b -> m c)
--apple :: (Misty m) => m a -> m (a -> b) -> m b
--furry' :: (Misty m) => (a -> b) -> m a -> m b
banana3 func w_a w_b w_c = apple w_c (banana2 func w_a w_b)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 func w_a w_b w_c w_d = apple w_d (banana3 func w_a w_b w_c)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  --furry :: (a -> b) -> f a -> f b
  --furry :: (a -> b) -> State (s -> (s, a)) -> State (s -> (s, b))
  furry func transform = State { state = (\initial_state -> let (second_state, value_a) = (state transform) initial_state
                                                                value_b = func value_a
                                                                in (second_state, value_b)) }

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  --banana :: (a -> m b) -> m a -> m b
  --banana :: (a -> State s b) -> State s a -> State s b
  banana func transform = State { state = (\initial_state -> let (second_state, value_a) = (state transform) initial_state
                                                                 second_transform = func value_a
                                                                 in (state second_transform) second_state) }
  unicorn x = State { state = (\s -> (s, x)) }

main = do return ()
