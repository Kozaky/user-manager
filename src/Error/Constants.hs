module Error.Constants where

import qualified Data.ByteString.Lazy as LB

-- |
--  Server Errors
serverError :: LB.ByteString
serverError = "server_error"

serverErrorMsg :: LB.ByteString
serverErrorMsg = "Internal Server Error"

-- |
--  User Errors
userNotFound :: LB.ByteString
userNotFound = "user_not_found"

alreadyUsedEmail :: LB.ByteString
alreadyUsedEmail = "user_already_used_email"

invalidEmail :: LB.ByteString
invalidEmail = "user_invalid_email"
