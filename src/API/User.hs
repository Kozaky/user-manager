module API.User (userAPI, UserAPI) where

import Colog (logError)
import qualified Data.Text as T
import Database (DbFailure (DbFailure))
import Error.Constants (generalErrorMsg, uuidAlreadyUsedEmail, uuidUserNotFound)
import Error.Utils (mkErrorMsg)
import Foundation (App)
import Servant (JSON, NoContent (NoContent), Put, ReqBody, ServerError (errBody), ServerT, err400, err404, err500, type (:>))
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
                "User not found"
          }

editUserH :: T.Text -> EditUserReq -> App NoContent
editUserH userId req = do
  result <- editUser userId req

  case result of
    Right () -> return NoContent
    Left (DbFailure 11000 _ Nothing) ->
      liftIO . throwIO $
        err400
          { errBody =
              mkErrorMsg
                uuidAlreadyUsedEmail
                "The email is already used"
          }
    Left (DbFailure code msg Nothing) -> do
      logError $
        "An error have been thrown during the updating with code: "
          <> (T.pack . show $ code)
          <> " and message: "
          <> T.pack msg
      liftIO . throwIO $ err500 {errBody = generalErrorMsg}
    Left (DbFailure _ _ (Just err)) -> do
      logError $ "An error have been thrown during the updating: " <> (T.pack . show $ err)
      liftIO . throwIO $
        err500
          { errBody = generalErrorMsg
          }
