module App.Context (Context (..), HasDbPool(..)) where

import Colog (LogAction, Message)
import Conferer (Config)
import Data.Pool (Pool)
import Database.Connection (Connection)

data Context m = Context
  { config :: !Config,
    dbPool :: !(Pool Connection),
    envLogAction :: !(LogAction m Message)
  }

class HasDbPool a where
  getDbPool :: a -> Pool Connection

instance HasDbPool (Pool Connection) where
  getDbPool = id

instance HasDbPool (Context m) where
  getDbPool = dbPool

