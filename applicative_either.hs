
module Applicator where
data Ether n y = Yes y | No n deriving (Show, Eq)

instance Functor (Ether n) where
  fmap f (Yes y) = Yes (f y)
  fmap f (No n) = No n

instance Applicative (Ether n) where
  pure = Yes
  _ <*> No n = No n
  Yes f <*> Yes y = Yes (f y)
  No f <*> _ = No f
  -- ???? this feels weird, i don't think i got this right

instance Monad (Ether n) where
  No n >>= _ = No n
  Yes y >>= f = f y

accept :: Ether String b -> Ether String b
accept thing = (Yes (+3)) <*> thing

reject :: Ether String b -> Ether String b
reject (Yes _) = No "way"
reject (No n) = No n
