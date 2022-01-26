module Context where

import Colog (HasLog (getLogAction, setLogAction), LogAction, Message)
import Conferer (Config)
import Database.Persist.MongoDB (ConnectionPool)

data Ctx m = Ctx
  { config :: !Config,
    dbPool :: !ConnectionPool,
    envLogAction :: !(LogAction m Message)
  }

instance HasLog (Ctx m) Message m where
  getLogAction :: Ctx m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Ctx m -> Ctx m
  setLogAction newLogAction ctx = ctx {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}
