
main = pure ()

data Predicate a = Predicate (a -> Bool)

fmurp :: (b -> a) -> Predicate a -> Predicate b
fmurp func (Predicate pf) = Predicate (pf . func)
