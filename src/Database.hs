module Database (mkPool, runQuery, QueryAction, Documentable (..)) where

import Colog (logError)
import Conferer (Config, fetchFromConfig)
import Context (Context (Context, dbPool))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT)
import qualified Data.Bson as Bson
import Data.Pool (Pool, createPool, withResource)
import qualified Data.Text as T
import Database.MongoDB.Connection (Host (Host), Pipe, close, connect, defaultPort)
import Database.MongoDB.Query (AccessMode (UnconfirmedWrites), Action, Failure, MongoContext, access, auth, master)
import DbConnection (DbAuth (DbAuth), DbConnection (DbConnection))
import Error.Constants (generalErrorMsg)
import Foundation (App)
import Servant (ServerError (errBody), err500)
import UnliftIO (MonadUnliftIO (withRunInIO), catch, throwIO)

mkPool :: Config -> IO (Pool DbConnection)
mkPool config = do
  dbName <- Conferer.fetchFromConfig "mongo.db.name" config
  dbHost <- Conferer.fetchFromConfig "mongo.db.host" config
  dbUser <- Conferer.fetchFromConfig "mongo.db.user" config
  dbPassword <- Conferer.fetchFromConfig "mongo.db.password" config
  dbPoolSize <- Conferer.fetchFromConfig "mongo.db.pool.size" config
  dbConns <- Conferer.fetchFromConfig "mongo.db.pool.connections" config
  dbMaxIdle :: Integer <- Conferer.fetchFromConfig "mongo.db.pool.iddle" config

  createPool
    (createPipe dbHost dbName (DbAuth dbUser dbPassword))
    (\(DbConnection pipe _) -> close pipe)
    dbPoolSize
    (fromInteger dbMaxIdle)
    dbConns

createPipe :: String -> T.Text -> DbAuth -> IO DbConnection
createPipe dbHost dbName dbAuth = do
  pipe <- connect (Host dbHost defaultPort)
  testAccess pipe dbName dbAuth
  return $ DbConnection pipe dbName

testAccess :: Pipe -> T.Text -> DbAuth -> IO ()
testAccess pipe dbName (DbAuth dbUser dbPassword) = do
  _ <- access pipe UnconfirmedWrites dbName (auth dbUser dbPassword)
  return ()

runQuery :: Action App a -> App a
runQuery action = do
  Context {dbPool} <- ask

  withRunInIO $ \run ->
    withResource dbPool $ \(DbConnection pipe db) -> run $ do
      catch
        (access pipe master db action)
        ( \(err :: Failure) -> do
            logError $ T.append "There has been a database error: " (T.pack . show $ err)
            throwIO $ err500 {errBody = generalErrorMsg}
        )

type QueryAction a = ReaderT MongoContext App a

class Documentable a where
  fromDocument :: Bson.Document -> a
  toDocument :: a -> Bson.Document
