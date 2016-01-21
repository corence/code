
class Ordinal t where
  lesser :: t -> t -> Bool
  greater :: t -> t -> Bool

instance Ordinal Int where
  lesser x y = x < y
  greater x y = x > y

getMin :: (Ordinal a) => a -> a -> a
getMin x y = if (lesser x y)
                then x
                else y

main = do
  let xx = 3 :: Int
  let yy = 6 :: Int
  let result = getMin xx yy
  print result
