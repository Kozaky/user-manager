module User.UserAPI (UserAPI, userAPI, CreateUser, GetUser, EditUser) where

import Colog (HasLog, Message, logError)
import Control.Monad.Reader (MonadReader)
import qualified Data.Text as T
import Error.ErrorTypes (APIError (toServantError), CustomServerError (InternalServerError))
import Servant (JSON, NoContent (NoContent), Put, ReqBody, ServerT, type (:>))
import Servant.API (Capture, Get, Post, type (:<|>) ((:<|>)))
import User.UserService (editUser, createUser, getUser)
import User.UserTypes (CreateUserReq, EditUserReq, UserDTO)
import UnliftIO (MonadIO, liftIO, throwIO)
import Prelude hiding (error)
import User.UserRepository (UserRepository)

type UserAPI =
  "users" :> (CreateUser :<|> GetUser :<|> EditUser)

-- type IndexUser
--   = Get '[JSON] [UserDTO]

type CreateUser = ReqBody '[JSON] CreateUserReq :> Post '[JSON] T.Text

type GetUser = Capture "id" T.Text :> Get '[JSON] UserDTO

type EditUser = Capture "id" T.Text :> ReqBody '[JSON] EditUserReq :> Put '[JSON] NoContent

userAPI :: (UserRepository m, MonadReader ctx m, HasLog ctx Message m, MonadIO m) => ServerT UserAPI m
userAPI = createUserH :<|> getUserH :<|> editUserH

createUserH :: (UserRepository m) => CreateUserReq -> m T.Text
createUserH = createUser

getUserH :: (UserRepository m, MonadIO m) => T.Text -> m UserDTO
getUserH userId = do
  result <- getUser userId

  case result of
    Right user -> return user
    Left error -> do
      liftIO . throwIO $ toServantError error

editUserH :: (UserRepository m, MonadReader ctx m, HasLog ctx Message m, MonadIO m) => T.Text -> EditUserReq -> m NoContent
editUserH userId req = do
  result <- editUser userId req

  case result of
    Right () -> return NoContent
    Left dbFailure -> do
      logError $
        "There has been an error during the updating with info: " <> (T.pack . show $ dbFailure)

      liftIO . throwIO $ toServantError InternalServerError
