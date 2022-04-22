module Error.Types where

import Data.ByteString.Lazy qualified as LB
import Error.Utils (mkErrorMsg)
import Servant (ServerError (ServerError, errBody, errHTTPCode, errHeaders, errReasonPhrase))

class ApiError e where
  httpCode :: e -> Int
  key :: e -> LB.ByteString
  msg :: e -> LB.ByteString

  toServantError :: e -> ServerError
  toServantError e =
    ServerError
      { errHTTPCode = httpCode e,
        errReasonPhrase = "",
        errBody = mkErrorMsg (key e) (msg e),
        errHeaders = []
      }

  toBody :: e -> LB.ByteString
  toBody e = mkErrorMsg (key e) (msg e)

data CustomServerError = InternalServerError
  deriving (Show)

instance ApiError CustomServerError where
  httpCode InternalServerError = 500

  key InternalServerError = "server_error"

  msg InternalServerError = "Internal Server Error"

data UserError = UserNotFoundError | InvalidEmailError
  deriving (Show)

instance ApiError UserError where
  httpCode UserNotFoundError = 404
  httpCode InvalidEmailError = 400

  key UserNotFoundError = "user_not_found"
  key InvalidEmailError = "user_invalid_email"

  msg UserNotFoundError = "User not found"
  msg InvalidEmailError = "Invalid Email"
