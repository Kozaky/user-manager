module Foundation (App (..), Environment) where

import Colog (HasLog (getLogAction, setLogAction), LogAction, Message, logError)
import Context (Context (..), envLogAction)
import Control.Monad.Reader (MonadIO, MonadReader (ask), ReaderT)
import Data.Pool (withResource)
import qualified Data.Text as T
import Database.MongoDB (Action, Failure, access, master)
import Error.Types (CustomServerError (InternalServerError), toServantError)
import Service.MongoDbManager (DbConnection (DbConnection), MongoDbManager (runQuery))
import UnliftIO (MonadUnliftIO (withRunInIO), catch, throwIO)

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

instance MongoDbManager App where
  runQuery :: Action App a -> App a
  runQuery = \action -> do
    Context {dbPool} <- ask

    withRunInIO $ \run ->
      withResource dbPool $ \(DbConnection pipe db) -> run $ do
        catch
          (access pipe master db action)
          ( \(err :: Failure) -> do
              logError $ T.append "There has been a database error: " (T.pack . show $ err)
              throwIO $ toServantError InternalServerError
          )

instance HasLog (Context App) Message App where
  getLogAction :: Context App -> LogAction App Message
  getLogAction Context {envLogAction} = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction App Message -> Context App -> Context App
  setLogAction newAction Context {..} = Context {envLogAction = newAction, ..}
  {-# INLINE setLogAction #-}
