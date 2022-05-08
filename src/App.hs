module App (run, server) where

import API (Api, api)
import API.User (userAPI)
import Colog (HasLog, Message, logTextStdout, (<&))
import Conferer (fetch, fetchFromConfig, mkConfig)
import Conferer.FromConfig.Warp ()
import Context (Context (Context))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Text (Text)
import Error.Middleware (catchErrorMiddleware)
import Foundation (App, unApp)
import Logger (mkLogger)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Prometheus (PrometheusSettings (PrometheusSettings), prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant (Application, Handler (Handler), HasServer (ServerT), hoistServer, layout, serve, type (:<|>) ((:<|>)))
import Service.MongoDbManager (MongoDbManager, mkPool)
import UnliftIO (MonadIO, try)

runApp :: Context App -> App a -> IO a
runApp ctx application = runReaderT (unApp application) ctx

server :: (Monad m, MongoDbManager m, MonadIO m, HasLog (Context m) Message m, MonadReader (Context m) m) => ServerT Api m
server = userAPI :<|> routesH

routesH :: Monad m => m Text
routesH = return $ layout api

app :: Context App -> Application
app ctx = serve api $ hoistServer api (Handler . ExceptT . try . runApp ctx) server

mkApp :: Context App -> IO Application
mkApp ctx = return $ app ctx

run :: IO ()
run = do
  cfg <- Conferer.mkConfig ""
  pool <- mkPool cfg
  logger <- mkLogger cfg

  let ctx = Context cfg pool logger :: Context App
  myApp <- mkApp ctx

  port <- Conferer.fetchFromConfig "server.port" cfg
  logTextStdout <& "Starting application on port: " <> port

  _ <- register ghcMetrics
  let promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True

  warpSettings :: Warp.Settings <- Conferer.fetch cfg
  Warp.runSettings warpSettings $ catchErrorMiddleware $ promMiddleware myApp
