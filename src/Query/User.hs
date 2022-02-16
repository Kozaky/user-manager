module Query.User (findUserByEmail, insertUser, findUserById, updateUser) where

import Data.Bson (Document, ObjectId, Value, (=:))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Database (DbFailure (DbFailure), QueryAction)
import Database.MongoDB.Query (Collection, Failure (WriteFailure), Select (select), UpdateOption (Upsert), WriteResult (WriteResult), findOne, insert, updateAll)
import Types.User (EditUserReq, Request (Request), unEmail)

userCollection :: Collection
userCollection = "user"

findUserByEmail :: T.Text -> QueryAction (Maybe Document)
findUserByEmail email = findOne (select ["email" =: email] userCollection)

insertUser :: Document -> QueryAction Value
insertUser = insert userCollection

findUserById :: ObjectId -> QueryAction (Maybe Document)
findUserById userId = findOne (select ["_id" =: userId] userCollection)

updateUser :: ObjectId -> EditUserReq -> QueryAction (Either DbFailure ())
updateUser userId req = do
  updateAll userCollection [(["_id" =: userId], updates, [Upsert])] <&> checkWriteResult
  where
    updates = mkUpdates req

checkWriteResult :: WriteResult -> Either DbFailure ()
checkWriteResult (WriteResult True _ _ _ _ [WriteFailure _ code msg] _) = Left $ DbFailure code msg Nothing
checkWriteResult (WriteResult True _ _ _ _ result _) = Left $ DbFailure (-1) "" (Just result)
checkWriteResult (WriteResult False _ _ _ _ _ _) = Right ()

mkUpdates :: EditUserReq -> Document
mkUpdates (Request name email password) = do
  let updates =
        maybe [] (\v -> ["name" =: v]) name
          ++ maybe [] (\v -> ["email" =: unEmail v]) email
          ++ maybe [] (\v -> ["password" =: v]) password

  ["$set" =: updates]
