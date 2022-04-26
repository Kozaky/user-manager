module API.User (userAPI, UserAPI, CreateUser, GetUser, EditUser) where

import Colog (logError)
import qualified Data.Text as T
import Error.Types (ApiError (toServantError), CustomServerError (InternalServerError))
import Foundation (App)
import Servant (JSON, NoContent (NoContent), Put, ReqBody, ServerT, type (:>))
import Servant.API (Capture, Get, Post, type (:<|>) ((:<|>)))
import Service.UserManager (createUser, editUser, getUser)
import Types.User (CreateUserReq, EditUserReq, UserDTO)
import UnliftIO (liftIO, throwIO)
import Prelude hiding (error)

type UserAPI =
  "users" :> (CreateUser :<|> GetUser :<|> EditUser)

-- type IndexUser
--   = Get '[JSON] [UserDTO]

type CreateUser = ReqBody '[JSON] CreateUserReq :> Post '[JSON] T.Text

type GetUser = Capture "id" T.Text :> Get '[JSON] UserDTO

type EditUser = Capture "id" T.Text :> ReqBody '[JSON] EditUserReq :> Put '[JSON] NoContent

userAPI :: ServerT UserAPI App
userAPI = createUserH :<|> getUserH :<|> editUserH

createUserH :: CreateUserReq -> App T.Text
createUserH = createUser

getUserH :: T.Text -> App UserDTO
getUserH userId = do
  result <- getUser userId

  case result of
    Right user -> return user
    Left error -> do
      liftIO . throwIO $ toServantError error

editUserH :: T.Text -> EditUserReq -> App NoContent
editUserH userId req = do
  result <- editUser userId req

  case result of
    Right () -> return NoContent
    Left dbFailure -> do
      logError $
        "There has been an error during the updating with info: " <> (T.pack . show $ dbFailure)

      liftIO . throwIO $ toServantError InternalServerError
