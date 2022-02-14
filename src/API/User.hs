module API.User (userAPI, UserAPI) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor (($>))
import qualified Data.Text as T
import Error.Constants (uuidUserNotFound)
import Error.Utils (mkErrorMsg)
import Foundation (App)
import Servant (JSON, NoContent (NoContent), Put, ReqBody, ServerError (errBody), ServerT, err404, type (:>))
import Servant.API (Capture, Get, Post, type (:<|>) ((:<|>)))
import Service.UserManager (createUser, editUser, getUser)
import Types.User (CreateUserReq, EditUserReq, UserDTO)
import UnliftIO (liftIO, throwIO)

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
    Left _ ->
      liftIO . throwIO $
        err404
          { errBody =
              mkErrorMsg
                uuidUserNotFound
                $ "No user found with id: " <> (B.pack . show) userId
          }

editUserH :: T.Text -> EditUserReq -> App NoContent
editUserH userId req = editUser userId req $> NoContent
