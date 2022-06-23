module Error.ErrorTypes where

import qualified Data.ByteString.Lazy as LB
import Error.Utils (mkErrorMsg)
import Servant (ServerError (ServerError, errBody, errHTTPCode, errHeaders, errReasonPhrase))

class APIError e where
  httpCode :: e -> Int
  errorCode :: e -> LB.ByteString
  errorMessage :: e -> LB.ByteString

  toServantError :: e -> ServerError
  toServantError e =
    ServerError
      { errHTTPCode = httpCode e,
        errReasonPhrase = "",
        errBody = mkErrorMsg (errorCode e) (errorMessage e),
        errHeaders = []
      }

  toBody :: e -> LB.ByteString
  toBody e = mkErrorMsg (errorCode e) (errorMessage e)

data CustomServerError = InternalServerError
  deriving (Show)

instance APIError CustomServerError where
  httpCode InternalServerError = 500

  errorCode InternalServerError = "server_error"

  errorMessage InternalServerError = "Internal Server Error"

data UserError = UserNotFoundError | InvalidEmailError
  deriving (Show)

instance APIError UserError where
  httpCode UserNotFoundError = 404
  httpCode InvalidEmailError = 400

  errorCode UserNotFoundError = "user_not_found"
  errorCode InvalidEmailError = "user_invalid_email"

  errorMessage UserNotFoundError = "User not found"
  errorMessage InvalidEmailError = "Invalid Email"
