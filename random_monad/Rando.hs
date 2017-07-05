import System.Random
import Control.Monad.State

rs :: State StdGen Int
rs = do
  (random, gen) <- random `fmap` get
  put gen
  return (random :: Int)

main = getStdGen >>= \g -> mapM_ print . flip evalState g . replicateM 4 $ rs

-- this is the same as `rs` but without the `do` notation -- this is what the compiler turns it into
rs2 :: State StdGen Int
rs2 = 
  random `fmap` get >>=
  (\(random, gen) -> put gen >>
  return (random :: Int))
