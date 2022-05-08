module MockApp (withApp) where

import API (api)
import App (server)
import Colog (HasLog (getLogAction, setLogAction), LogAction, Message, logError)
import qualified Conferer
import Context (Context (..), dbPool, envLogAction)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Pool (withResource)
import qualified Data.Text as T
import Database.MongoDB (Action, Failure, access, master)
import Error.Types (CustomServerError (InternalServerError), toServantError)
import Foundation (Environment)
import Logger (mkLogger)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Handler (Handler), hoistServer, serve)
import Service.MongoDbManager (DbConnection (DbConnection), MongoDbManager (runQuery), mkPool)
import UnliftIO (MonadIO, MonadUnliftIO (withRunInIO), catch, throwIO, try)

-- | Data type representing the most general effects our application should be
-- able to perform.
newtype MockedApp a = MockedApp {unMockedApp :: Environment MockedApp a}
  deriving newtype
    ( Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader (Context MockedApp),
      Functor
    )

instance MongoDbManager MockedApp where
  runQuery :: Action MockedApp a -> MockedApp a
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

instance HasLog (Context MockedApp) Message MockedApp where
  getLogAction :: Context MockedApp -> LogAction MockedApp Message
  getLogAction Context {envLogAction} = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction MockedApp Message -> Context MockedApp -> Context MockedApp
  setLogAction newAction Context {..} = Context {envLogAction = newAction, ..}
  {-# INLINE setLogAction #-}

mkApp :: IO Application
mkApp = do
  cfg <- Conferer.mkConfig $ T.pack ""
  pool <- mkPool cfg
  logger <- mkLogger cfg

  let ctx = Context cfg pool logger
  return $ app ctx

runApp :: Context MockedApp -> MockedApp a -> IO a
runApp ctx application = runReaderT (unMockedApp application) ctx

app :: Context MockedApp -> Application
app ctx = serve api $ hoistServer api (Handler . ExceptT . try . runApp ctx) server

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication mkApp
