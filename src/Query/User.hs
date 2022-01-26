module Query.User (findUserByEmail, insertUser, findUserById, updateUser) where

import Control.Exception (SomeException)
import Data.Text (Text)
import Database (QueryAction)
import Database.Persist.Sql
  ( Entity,
    PersistEntity (Key),
    PersistQueryRead (selectFirst),
    PersistStoreRead (get),
    PersistStoreWrite (insert, updateGet),
    Update,
    (=.),
    (==.),
  )
import Model.User (EntityField (UserEmail, UserName, UserPassword), User)
import Types.User (EditUserReq, Request (Request), emailAsText)
import UnliftIO.Exception (tryAny)

findUserByEmail :: Text -> QueryAction (Maybe (Entity User))
findUserByEmail email = selectFirst [UserEmail ==. email] []

insertUser :: User -> QueryAction (Key User)
insertUser = insert

findUserById :: Key User -> QueryAction (Maybe User)
findUserById = get

updateUser :: Key User -> EditUserReq -> QueryAction (Either SomeException User)
updateUser userId req = do
  let updates = mkUpdates req
  tryAny $ updateGet userId updates

mkUpdates :: EditUserReq -> [Update User]
mkUpdates (Request name email password) =
  maybe [] (\v -> [UserName =. v]) name
    ++ maybe [] (\v -> [UserEmail =. emailAsText v]) email
    ++ maybe [] (\v -> [UserPassword =. v]) password
