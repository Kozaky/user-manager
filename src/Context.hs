{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Context where

import Colog (HasLog (getLogAction, setLogAction), LogAction, Message)
import Conferer (Config)
import Control.Lens (makeLenses)
import Database.Persist.MongoDB (ConnectionPool)

data Ctx m = Ctx
  { _config :: !Config,
    _dbPool :: !ConnectionPool,
    _envLogAction :: !(LogAction m Message)
  }

-- | Generate lenses for our @Env@.
makeLenses ''Ctx

instance HasLog (Ctx m) Message m where
  getLogAction :: Ctx m -> LogAction m Message
  getLogAction = _envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Ctx m -> Ctx m
  setLogAction newLogAction ctx = ctx {_envLogAction = newLogAction}
  {-# INLINE setLogAction #-}
