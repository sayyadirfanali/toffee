module Utils where

import Control.Monad.IO.Class

printIO :: (Show a, MonadIO m) => a -> m ()
printIO = liftIO . print