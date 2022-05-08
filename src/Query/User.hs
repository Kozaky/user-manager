module Query.User (findUserByEmail, insertUser, findUserById, updateUser) where

import Data.Bson (Document, ObjectId, Value, (=:))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Database.MongoDB.Query (Collection, Select (select), UpdateOption (Upsert), findOne, insert, updateAll)
import Service.MongoDbManager (DbFailure, QueryAction, checkWriteResult)
import Types.User (EditUserReq, Request (Request), unEmail)
import UnliftIO (MonadIO)

userCollection :: Collection
userCollection = "user"

findUserByEmail :: (MonadIO m) => T.Text -> QueryAction m (Maybe Document)
findUserByEmail email = findOne (select ["email" =: email] userCollection)

insertUser :: (MonadIO m) => Document -> QueryAction m Value
insertUser = insert userCollection

findUserById :: (MonadIO m) => ObjectId -> QueryAction m (Maybe Document)
findUserById userId = findOne (select ["_id" =: userId] userCollection)

updateUser :: (MonadIO m) => ObjectId -> EditUserReq -> QueryAction m (Either DbFailure ())
updateUser userId req = do
  updateAll userCollection [(["_id" =: userId], updates, [Upsert])] <&> checkWriteResult
  where
    updates = mkUpdates req

mkUpdates :: EditUserReq -> Document
mkUpdates (Request name email password) = do
  let updates =
        maybe [] (\v -> ["name" =: v]) name
          ++ maybe [] (\v -> ["email" =: unEmail v]) email
          ++ maybe [] (\v -> ["password" =: v]) password

  ["$set" =: updates]
