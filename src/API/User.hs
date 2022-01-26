module API.User (userAPI, UserAPI) where

import qualified Data.ByteString.Lazy.Char8 as B
import Database.Persist (Key)
import Database.Persist.MongoDB (keyToOid)
import Error.Constants (uuidAlreadyUsedEmail, uuidUserNotFound)
import Error.Utils (mkErrorMsg)
import Foundation (App)
import Model.User (User)
import Servant (JSON, Put, ReqBody, ServerError (errBody), ServerT, err400, err404, type (:>))
import Servant.API (Capture, Get, Post, type (:<|>) ((:<|>)))
import Service.UserManager (createUser, editUser, getUser)
import Types.User (CreateUserReq, EditUserReq, UserDTO, userDTOfromUser, userFromCreateUserReq)
import UnliftIO (liftIO, throwIO)

type UserAPI =
  "users" :> (CreateUser :<|> GetUser :<|> EditUser)

-- type IndexUser
--   = Get '[JSON] [UserDTO]

type CreateUser = ReqBody '[JSON] CreateUserReq :> Post '[JSON] (Key User)

type GetUser = Capture "id" (Key User) :> Get '[JSON] UserDTO

type EditUser = Capture "id" (Key User) :> ReqBody '[JSON] EditUserReq :> Put '[JSON] UserDTO

userAPI :: ServerT UserAPI App
userAPI = createUserH :<|> getUserH :<|> editUserH

createUserH :: CreateUserReq -> App (Key User)
createUserH req = do
  result <- createUser $ userFromCreateUserReq req

  case result of
    Right key -> return key
    Left _ -> liftIO . throwIO $ err400 {errBody = mkErrorMsg uuidAlreadyUsedEmail "Email already used"}

getUserH :: Key User -> App UserDTO
getUserH userId = do
  result <- getUser userId

  case result of
    Right user -> return $ userDTOfromUser (userId, user)
    Left _ ->
      liftIO . throwIO $
        err404
          { errBody =
              mkErrorMsg
                uuidUserNotFound
                $ "No user found with id: " <> (B.pack . show . keyToOid) userId
          }

editUserH :: Key User -> EditUserReq -> App UserDTO
editUserH userId req = do
  result <- editUser userId req

  case result of
    Right user -> return $ userDTOfromUser (userId, user)
    Left msg ->
      liftIO . throwIO $
        err404
          { errBody =
              mkErrorMsg
                uuidUserNotFound
                $ B.pack . show $ msg
          }
