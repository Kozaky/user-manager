module API.Utils (withApp) where

import API (api)
import App (server)
import qualified Conferer
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as T
import Foundation (App, unApp)
import Logger (mkLogger)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Handler (Handler), hoistServer, serve)
import Service.MongoDbManager (mkPool)
import Context (Context (Context))
import UnliftIO (try)

mkApp :: IO Application
mkApp = do
  cfg <- Conferer.mkConfig $ T.pack ""
  pool <- mkPool cfg
  logger <- mkLogger cfg

  let ctx = Context cfg pool logger
  return $ app ctx

runApp :: Context App -> App a -> IO a
runApp ctx application = runReaderT (unApp application) ctx

app :: Context App -> Application
app ctx = serve api $ hoistServer api (Handler . ExceptT . try . runApp ctx) server

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication mkApp