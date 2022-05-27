module Service.UserManager (getUser, createUser, editUser) where

import qualified Data.Text as T
import Error.Types (UserError (UserNotFoundError))
import Repo.User (UserRepository(..))
import Service.MongoDbManager (DbFailure, Documentable (fromDocument, toDocument))
import Types.User (CreateUserReq, EditUserReq, UserDTO, userDTOfromUser, userFromCreateUserReq)

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

editUser :: (UserRepository m) => T.Text -> EditUserReq -> m (Either DbFailure ())
editUser userId = update (read . T.unpack $ userId)
