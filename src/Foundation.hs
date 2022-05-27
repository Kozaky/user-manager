module Foundation (App (..), Environment) where

import Colog (HasLog (getLogAction, setLogAction), LogAction, Message)
import Context (Context (..), envLogAction)
import Control.Monad.Reader (MonadReader, ReaderT)
import UnliftIO (MonadIO, MonadUnliftIO)

type Environment m = ReaderT (Context m) IO

-- | Data type representing the most general effects our application should be
-- able to perform.
newtype App a = App {unApp :: Environment App a}
  deriving newtype
    ( Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader (Context App),
      Functor
    )

instance HasLog (Context App) Message App where
  getLogAction :: Context App -> LogAction App Message
  getLogAction Context {envLogAction} = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction App Message -> Context App -> Context App
  setLogAction newAction Context {..} = Context {envLogAction = newAction, ..}
  {-# INLINE setLogAction #-}
