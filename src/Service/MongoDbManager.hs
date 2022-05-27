module Service.MongoDbManager (mkPool, checkWriteResult, QueryAction, Documentable (..), DbFailure (..), runQuery) where

import Conferer (Config, fetchFromConfig)
import Control.Monad.Reader (ReaderT, void, MonadReader (ask))
import qualified Data.Bson as Bson
import Data.Pool (Pool, createPool, withResource)
import qualified Data.Text as T
import Database.MongoDB.Connection (Host (Host), Pipe, close, connect, defaultPort)
import Database.MongoDB.Query (Failure (WriteFailure), WriteResult (WriteResult), access, auth, master, Action, MongoContext)
import DbConnection (DbAuth (..), DbConnection (..))
import Context (HasDbPool (getDbPool))
import UnliftIO (withRunInIO, MonadUnliftIO, catch, throwIO)
import Colog (HasLog, Message, logError)
import Error.Types (ApiError(toServantError), CustomServerError (InternalServerError))

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
    (createConnection dbHost dbName (DbAuth dbUser dbPassword))
    (\(DbConnection pipe _) -> close pipe)
    dbPoolSize
    (fromInteger dbMaxIdle)
    dbConns

createConnection :: String -> T.Text -> DbAuth -> IO DbConnection
createConnection dbHost dbName dbAuth = do
  pipe <- connect (Host dbHost defaultPort)
  testAccess pipe dbName dbAuth
  return $ DbConnection pipe dbName

testAccess :: Pipe -> T.Text -> DbAuth -> IO ()
testAccess pipe dbName (DbAuth dbUser dbPassword) =
  void $ access pipe master dbName (auth dbUser dbPassword)

runQuery :: (MonadReader ctx m, Monad m, HasLog ctx Message m, HasDbPool ctx, MonadUnliftIO m) => Action m a -> m a
runQuery action = do
  ctx <- ask

  withRunInIO $ \run ->
    withResource (getDbPool ctx) $ \(DbConnection pipe db) -> run $ do
      catch
        (access pipe master db action)
        ( \(err :: Failure) -> do
            logError $ T.append "There has been a database error: " (T.pack . show $ err)
            throwIO $ toServantError InternalServerError
        )

data DbFailure = DbFailure {code :: Int, msg :: String, result :: Maybe [Failure]} deriving (Show)

checkWriteResult :: WriteResult -> Either DbFailure ()
checkWriteResult (WriteResult True _ _ _ _ [WriteFailure _ code msg] _) = Left $ DbFailure code msg Nothing
checkWriteResult (WriteResult True _ _ _ _ result _) = Left $ DbFailure (-1) "" (Just result)
checkWriteResult (WriteResult False _ _ _ _ _ _) = Right ()

type QueryAction m a = ReaderT MongoContext m a

class Documentable a where
  fromDocument :: Bson.Document -> a
  toDocument :: a -> Bson.Document
