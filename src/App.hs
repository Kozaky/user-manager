{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module App (run) where

import Api (Api, api)
import Colog (Message, Severity (Debug, Info), WithLog, log, richMessageAction)
import Conferer (fetch, mkConfig)
import Conferer.FromConfig.Warp ()
import Context (Ctx (Ctx), config, dbPool)
import Control.Exception (try)
import Control.Lens (view)
import Control.Lens.TH ()
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Database.MongoDB.Connection (defaultPort)
import Database.Persist.MongoDB
  ( ConnectionPool,
    MongoAuth (MongoAuth),
    PersistEntity (Key),
    createMongoDBPool,
    entityVal,
    insert,
    runMongoDBPoolDef,
    selectFirst,
    (==.),
  )
import Models (EntityField (UserName), User (userName))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus (PrometheusSettings (PrometheusSettings), prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant (Application, Handler (Handler), HasServer (ServerT), hoistServer, serve, type (:<|>) ((:<|>)))
import UnliftIO (MonadUnliftIO)
import Prelude hiding (log)

-- | Concrete representation of our app's transformer stack.
-- type App a = ReaderT (Ctx (AppT IO)) IO a

-- | Data type representing the most general effects our application should be
-- able to perform.
newtype App a = App
  { unApp :: ReaderT (Ctx App) IO a
  }
  deriving newtype (Functor)
  deriving newtype
    ( Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader (Ctx App)
    )

-- | Run an 'AppT' using the given 'Ctx'.
runApp :: Ctx App -> App a -> IO a
runApp ctx application = runReaderT (unApp application) ctx

server :: ServerT Api App
server = do
  userAddH :<|> userGetH :<|> holaH
  where
    userAddH newUser = userAdd newUser
    userGetH name = userGet name

    userAdd :: User -> App (Maybe (Key User))
    userAdd newUser = do
      pool <- view dbPool
      flip runMongoDBPoolDef pool $ do
        exists <- selectFirst [UserName ==. userName newUser] []
        case exists of
          Nothing -> Just <$> insert newUser
          Just _ -> return Nothing

    userGet :: Text -> App (Maybe User)
    userGet name = do
      pool <- view dbPool
      cfg <- view config
      warpSettings <- (liftIO . Conferer.fetch) cfg
      exampleLog (Warp.getPort warpSettings)
      flip runMongoDBPoolDef pool $ do
        mUser <- selectFirst [UserName ==. name] []
        return $ entityVal <$> mUser

holaH :: App (Maybe String)
holaH = return $ Just "hola"

app :: Ctx App -> Application
app env = serve api $ hoistServer api (Handler . ExceptT . try . runApp env) server

mkApp :: Ctx App -> IO Application
mkApp env = return $ app env

run :: IO ()
run = do
  !cfg <- Conferer.mkConfig ""
  !pool <- mkPool

  warpSettings :: Warp.Settings <- Conferer.fetch cfg
  let ctx = Ctx cfg pool richMessageAction
  myApp <- mkApp ctx
  _ <- register ghcMetrics
  let promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True

  Warp.runSettings warpSettings $ promMiddleware myApp

exampleLog :: WithLog env Message m => Int -> m ()
exampleLog port = do
  log Debug $ Text.append "First message... Running on port: " (T.pack . show $ port)
  log Info $ Text.append "Second message... Running on port: " (T.pack . show $ port)

mkPool :: IO ConnectionPool
mkPool =
  createMongoDBPool
    "holajobs-users"
    "mongo"
    defaultPort
    (Just $ MongoAuth "holajobs-users" "holajobs-users")
    10
    10
    30
