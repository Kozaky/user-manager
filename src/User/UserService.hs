module User.UserService (getUser, createUser, editUser) where

import qualified Data.Text as T
import Error.ErrorTypes (UserError (UserNotFoundError))
import User.UserRepository (UserRepository(..))
import Database.MongoDBService (DBFailure, Documentable (fromDocument, toDocument))
import User.UserTypes (CreateUserReq, EditUserReq, UserDTO, userDTOfromUser, userFromCreateUserReq)

createUser :: (UserRepository m) => CreateUserReq -> m T.Text
createUser req = do
  let newUser = toDocument $ userFromCreateUserReq req
  userId <- save newUser

  return . T.pack . show $ userId

getUser :: (UserRepository m) => T.Text -> m (Either UserError UserDTO)
getUser userId = do
  mDoc <- find $ read . T.unpack $ userId

  case mDoc of
    Just doc -> return . Right . userDTOfromUser . fromDocument $ doc
    Nothing -> return $ Left UserNotFoundError

editUser :: (UserRepository m) => T.Text -> EditUserReq -> m (Either DBFailure ())
editUser userId = update (read . T.unpack $ userId)
