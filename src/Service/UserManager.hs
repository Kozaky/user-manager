module Service.UserManager (getUser, createUser, editUser) where

import Data.Functor ((<&>))
import qualified Data.Text as T
import Error.Types (UserError (UserNotFoundError))
import Query.User
  ( findUserById,
    insertUser,
    updateUser,
  )
import Service.MongoDbManager (DbFailure, Documentable (fromDocument, toDocument), MongoDbManager, runQuery)
import Types.User (CreateUserReq, EditUserReq, UserDTO, userDTOfromUser, userFromCreateUserReq)
import UnliftIO (MonadIO)

createUser :: (MongoDbManager m, MonadIO m) => CreateUserReq -> m T.Text
createUser req =
  runQuery $
    insertUser (toDocument $ userFromCreateUserReq req) <&> (T.pack . show)

getUser :: (MongoDbManager m, MonadIO m) => T.Text -> m (Either UserError UserDTO)
getUser userId = do
  runQuery $ do
    mDoc <- findUserById $ read . T.unpack $ userId

    case mDoc of
      Just doc -> return . Right . userDTOfromUser . fromDocument $ doc
      Nothing -> return $ Left UserNotFoundError

editUser :: (MongoDbManager m, MonadIO m) => T.Text -> EditUserReq -> m (Either DbFailure ())
editUser userId req = runQuery $ updateUser (read . T.unpack $ userId) req
