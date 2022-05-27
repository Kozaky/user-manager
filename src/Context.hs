module Context (Context (..), HasDbPool(..)) where

import Colog (LogAction, Message)
import Conferer (Config)
import Data.Pool (Pool)
import DbConnection (DbConnection)

data Context m = Context
  { config :: !Config,
    dbPool :: !(Pool DbConnection),
    envLogAction :: !(LogAction m Message)
  }

class HasDbPool a where
  getDbPool :: a -> Pool DbConnection

instance HasDbPool (Pool DbConnection) where
  getDbPool = id

instance HasDbPool (Context m) where
  getDbPool = dbPool

