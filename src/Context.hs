module Context (Context (..), Database (..)) where

import Colog (HasLog (getLogAction, setLogAction), LogAction, Message)
import Conferer (Config)
import Control.Monad.Reader (MonadReader)
import Data.Pool (Pool)
import Database.MongoDB (Action)
import DbConnection (DbConnection)
import UnliftIO (MonadUnliftIO)

data Context m = forall store.
  (Database store) =>
  Context
  { config :: !Config,
    dbPool :: !(Pool DbConnection),
    envLogAction :: !(LogAction m Message),
    database :: store
  }

instance HasLog (Context m) Message m where
  getLogAction :: Context m -> LogAction m Message
  getLogAction Context {envLogAction} = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Context m -> Context m
  setLogAction newAction Context {..} = Context {envLogAction = newAction, ..}
  {-# INLINE setLogAction #-}

class Database store where
  runQuery' :: (Monad m, HasLog (Context m) Message m, MonadReader (Context m) m, MonadUnliftIO m) => store -> Action m a -> m a
