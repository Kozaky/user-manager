module App (run) where

import API (Api, api)
import API.User (userAPI)
import Colog (logTextStdout, (<&))
import Conferer (fetch, fetchFromConfig, mkConfig)
import Conferer.FromConfig.Warp ()
import Context (Ctx (Ctx))
import Control.Exception (try)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text (Text)
import Database (mkPool)
import Error.Middleware (catchErrorMiddleware)
import Foundation (App (unApp))
import Logger (mkLogger)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Prometheus (PrometheusSettings (PrometheusSettings), prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant (Application, Handler (Handler), HasServer (ServerT), hoistServer, layout, serve, type (:<|>) ((:<|>)))

runApp :: Ctx App -> App a -> IO a
runApp ctx application = runReaderT (unApp application) ctx

server :: ServerT Api App
server = userAPI :<|> routesH

routesH :: App Text
routesH = do
  return $ layout api

app :: Ctx App -> Application
app ctx = serve api $ hoistServer api (Handler . ExceptT . try . runApp ctx) server

mkApp :: Ctx App -> IO Application
mkApp ctx = return $ app ctx

run :: IO ()
run = do
  cfg <- Conferer.mkConfig ""
  pool <- mkPool cfg
  logger <- mkLogger cfg

  let ctx = Ctx cfg pool logger
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
