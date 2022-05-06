{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App (run, app) where

import API (Api, api)
import API.User (userAPI)
import Colog (HasLog, Message, logError, logTextStdout, (<&))
import Conferer (fetch, fetchFromConfig, mkConfig)
import Conferer.FromConfig.Warp ()
import Context (Context (Context), Database (runQuery'), dbPool)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Database (mkPool)
import Database.MongoDB
import DbConnection
import Error.Middleware (catchErrorMiddleware)
import Error.Types (ApiError (toServantError), CustomServerError (InternalServerError))
import Foundation (App (unApp))
import Logger (mkLogger)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Prometheus (PrometheusSettings (PrometheusSettings), prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant (Application, Handler (Handler), HasServer (ServerT), hoistServer, layout, serve, type (:<|>) ((:<|>)))
import UnliftIO (MonadUnliftIO (withRunInIO), catch, throwIO, try)

runApp :: Context App -> App a -> IO a
runApp ctx application = runReaderT (unApp application) ctx

server :: ServerT Api App
server = userAPI :<|> routesH

routesH :: App Text
routesH = do
  return $ layout api

app :: Context App -> Application
app ctx = serve api $ hoistServer api (Handler . ExceptT . try . runApp ctx) server

mkApp :: Context App -> IO Application
mkApp ctx = return $ app ctx

data Db = Db

instance Context.Database Db where
  runQuery' :: (Monad m, HasLog (Context m) Message m, MonadReader (Context m) m, MonadUnliftIO m) => store -> Action m a -> m a
  runQuery' = \_db action -> do
    Context {dbPool} <- ask

    withRunInIO $ \run ->
      withResource dbPool $ \(DbConnection pipe db) -> run $ do
        catch
          (access pipe master db action)
          ( \(err :: Failure) -> do
              logError $ T.append "There has been a database error: " (T.pack . show $ err)
              throwIO $ toServantError InternalServerError
          )

run :: IO ()
run = do
  cfg <- Conferer.mkConfig ""
  pool <- mkPool cfg
  logger <- mkLogger cfg

  let db = Db
  let ctx = Context cfg pool logger db
  myApp <- mkApp ctx

  port <- Conferer.fetchFromConfig "server.port" cfg
  logTextStdout <& "Starting application on port: " <> port

  _ <- register ghcMetrics
  let promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True

  warpSettings :: Warp.Settings <- Conferer.fetch cfg
  Warp.runSettings warpSettings $ catchErrorMiddleware $ promMiddleware myApp

-- exampleLog :: Int -> App ()
-- exampleLog port = do
--   logDebug  $ Text.append "First message... Running on port: " (T.pack . show $ port)
--   logInfo $ Text.append "Second message... Running on port: " (T.pack . show $ port)
--   logError $ Text.append "Second message... Running on port: " (T.pack . show $ port)
