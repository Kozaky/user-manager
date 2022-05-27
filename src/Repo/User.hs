module Repo.User (findUserByEmail, insertUser, findUserById, updateUser, UserRepository(..)) where

import Data.Bson (Document, ObjectId, Value, (=:))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Database.MongoDB.Query (Collection, Select (select), UpdateOption (Upsert), findOne, insert, updateAll)
import Service.MongoDbManager (DbFailure, QueryAction, checkWriteResult, runQuery)
import Types.User (EditUserReq, Request (Request), Email, pattern Email)
import UnliftIO (MonadIO, MonadUnliftIO)
import Colog (Message, HasLog)
import Control.Monad.Reader (MonadReader)
import Context (HasDbPool)
import Foundation (App)

class (Monad m) => UserRepository m where
  find :: ObjectId ->  m (Maybe Document)
  save :: Document -> m Value
  update :: ObjectId -> EditUserReq -> m (Either DbFailure ())

instance UserRepository App where
  find = findUserById
  save = insertUser
  update = updateUser

userCollection :: Collection
userCollection = "user"

findUserByEmail :: (MonadIO m) => T.Text -> QueryAction m (Maybe Document)
findUserByEmail email = findOne (select ["email" =: email] userCollection)

insertUser :: (HasLog ctx Message m, HasDbPool ctx, MonadReader ctx m, MonadIO m, MonadUnliftIO m) => Document -> m Value
insertUser doc = runQuery $ insert userCollection doc

findUserById :: (HasLog ctx Message m, HasDbPool ctx, MonadReader ctx m, MonadIO m, MonadUnliftIO m) => ObjectId -> m (Maybe Document)
findUserById userId = runQuery $ findOne (select ["_id" =: userId] userCollection)

updateUser :: (HasLog ctx Message m, HasDbPool ctx, MonadReader ctx m, MonadIO m, MonadUnliftIO m) => ObjectId -> EditUserReq -> m (Either DbFailure ())
updateUser userId req =
  runQuery $ updateAll userCollection [(["_id" =: userId], updates, [Upsert])] <&> checkWriteResult
  where
    updates :: Document
    updates = mkUpdates req

mkUpdates :: EditUserReq -> Document
mkUpdates (Request userName userEmail userPassword) =
  ["$set" =: mkUserUpdates userName userEmail userPassword]
  where 
    mkUserUpdates :: Maybe T.Text -> Maybe Email -> Maybe T.Text -> Document
    mkUserUpdates name email password = 
      maybe [] (\v -> ["name" =: v]) name
            ++ maybe [] (\(Email v) -> ["email" =: v]) email
            ++ maybe [] (\v -> ["password" =: v]) password
