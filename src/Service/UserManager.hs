module Service.UserManager (getUser, createUser, editUser) where

import Control.Exception (SomeException)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Database (runQuery)
import Foundation (App)
import Model.User (Key, User (..))
import Query.User
  ( findUserByEmail,
    findUserById,
    insertUser,
    updateUser,
  )
import Types.User (EditUserReq)

createUser :: User -> App (Either T.Text (Key User))
createUser newUser = do
  runQuery $ do
    exists <- findUserByEmail $ userEmail newUser
    case exists of
      Nothing -> insertUser newUser <&> Right
      Just _ -> return $ Left "Email already used"

getUser :: Key User -> App (Either T.Text User)
getUser userId = do
  runQuery $ do
    mUser <- findUserById userId

    case mUser of
      Just user -> return $ Right user
      Nothing -> return $ Left "User not found"

editUser :: Key User -> EditUserReq -> App (Either SomeException User)
editUser userId req = do
  runQuery $ do
    mUser <- updateUser userId req

    case mUser of
      Right user -> return $ Right user
      Left e -> return $ Left e
