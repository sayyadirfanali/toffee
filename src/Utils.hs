module Utils where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.IO
import Text.Megaparsec

printIO :: (Show a, MonadIO m) => a -> m ()
printIO = liftIO . print

isFollowedBy :: MonadParsec e s m => m a -> m Bool
isFollowedBy p = do
  b <- optional (lookAhead p)
  return (isJust b)

repl = replicate