
module Print where

import Control.Monad.IO.Class

class Printy a where
    putStr :: MonadIO m => a -> m ()
    putStrLn :: MonadIO m => a -> m ()
    
instance Printy String where
    putStr = liftIO . Prelude.putStr
    putStrLn = liftIO . Prelude.putStrLn
