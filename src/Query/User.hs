module Query.User (findUserByEmail, insertUser, findUserById, updateUser) where

import Data.Bson (Document, ObjectId, Value, (=:))
import qualified Data.Text as T
import Database (QueryAction)
import Database.MongoDB.Query (Select (select), findOne, insert, modify)
import Types.User (EditUserReq, Request (Request), unEmail)

findUserByEmail :: T.Text -> QueryAction (Maybe Document)
findUserByEmail email = findOne (select ["email" =: email] "user")

insertUser :: Document -> QueryAction Value
insertUser = insert "user"

findUserById :: ObjectId -> QueryAction (Maybe Document)
findUserById userId = findOne (select ["_id" =: userId] "user")

updateUser :: ObjectId -> EditUserReq -> QueryAction ()
updateUser userId req = do
  let updates = mkUpdates req
  modify (select ["_id" =: userId] "user") updates

mkUpdates :: EditUserReq -> Document
mkUpdates (Request name email password) = do
  let updates =
        maybe [] (\v -> ["name" =: v]) name
          ++ maybe [] (\v -> ["email" =: unEmail v]) email
          ++ maybe [] (\v -> ["password" =: v]) password

  ["$set" =: updates]
