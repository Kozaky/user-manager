module Context (Context (..)) where

import Colog (LogAction, Message)
import Conferer (Config)
import Data.Pool (Pool)
import Service.MongoDbManager (DbConnection)

data Context m = Context
  { config :: !Config,
    dbPool :: !(Pool DbConnection),
    envLogAction :: !(LogAction m Message)
  }
