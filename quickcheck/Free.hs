
module Free where

newtype Free a = Free a

instance Functor Free where
    fmap aToB (Free a) = Free (aToB a)
    
instance Applicative Free where
    pure a = Free a
    (<*>) (Free aToB) (Free a) = Free (aToB a)
    
instance Monad Free where
    (>>=) (Free a) aToFB = aToFB a
