module Context (Context (..)) where

import Colog (HasLog (getLogAction, setLogAction), LogAction, Message)
import Conferer (Config)
import Data.Pool (Pool)
import DbConnection (DbConnection)

data Context m = Context
  { config :: !Config,
    dbPool :: !(Pool DbConnection),
    envLogAction :: !(LogAction m Message)
  }

instance HasLog (Context m) Message m where
  getLogAction :: Context m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Context m -> Context m
  setLogAction newLogAction context = context {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}
