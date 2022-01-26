module Database (mkPool, runQuery, QueryAction) where

import Conferer (Config, fetchFromConfig)
import Context (Ctx (Ctx, dbPool))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT)
import Database.MongoDB (MongoContext, defaultPort)
import Database.Persist.MongoDB (Action, ConnectionPool, MongoAuth (MongoAuth), createMongoDBPool, runMongoDBPoolDef)
import Foundation (App)

mkPool :: Config -> IO ConnectionPool
mkPool cfg = do
  dbName <- Conferer.fetchFromConfig "mongo.db.name" cfg
  dbHost <- Conferer.fetchFromConfig "mongo.db.host" cfg
  dbUser <- Conferer.fetchFromConfig "mongo.db.user" cfg
  dbPwd <- Conferer.fetchFromConfig "mongo.db.password" cfg
  dbPoolSize <- Conferer.fetchFromConfig "mongo.db.pool.size" cfg
  dbConns <- Conferer.fetchFromConfig "mongo.db.pool.connections" cfg
  dbMaxIdle :: Integer <- Conferer.fetchFromConfig "mongo.db.pool.iddle" cfg

  createMongoDBPool
    dbName
    dbHost
    defaultPort
    (Just $ MongoAuth dbUser dbPwd)
    dbPoolSize
    dbConns
    $ fromInteger dbMaxIdle

type QueryAction a = ReaderT MongoContext App a

runQuery :: Action App a -> App a
runQuery action = do
  Ctx {dbPool} <- ask
  runMongoDBPoolDef action dbPool
