module Service.UserManager (getUser, createUser, editUser) where

import Data.Functor ((<&>))
import qualified Data.Text as T
import Database (DbFailure, Documentable (fromDocument, toDocument), runQuery)
import Error.Types (UserError (UserNotFoundError))
import Foundation (App)
import Query.User
  ( findUserById,
    insertUser,
    updateUser,
  )
import Types.User (CreateUserReq, EditUserReq, UserDTO, userDTOfromUser, userFromCreateUserReq)

createUser :: CreateUserReq -> App T.Text
createUser req =
  runQuery $
    insertUser (toDocument $ userFromCreateUserReq req) <&> (T.pack . show)

getUser :: T.Text -> App (Either UserError UserDTO)
getUser userId = do
  runQuery $ do
    mDoc <- findUserById $ read . T.unpack $ userId

    case mDoc of
      Just doc -> return . Right . userDTOfromUser . fromDocument $ doc
      Nothing -> return $ Left UserNotFoundError

editUser :: T.Text -> EditUserReq -> App (Either DbFailure ())
editUser userId req = runQuery $ updateUser (read . T.unpack $ userId) req
