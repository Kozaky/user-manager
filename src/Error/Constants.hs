module Error.Constants (uuidGeneralError, generalErrorMsg, uuidUserNotFound, uuidAlreadyUsedEmail, uuidInvalidEmail) where

import qualified Data.ByteString.Lazy as LB

uuidGeneralError :: LB.ByteString
uuidGeneralError = "1e799e9d-ff52-42f5-9165-b73780b38270"

generalErrorMsg :: LB.ByteString
generalErrorMsg = "Server Error!!"

uuidUserNotFound :: LB.ByteString
uuidUserNotFound = "1be010e4-e8b9-442f-a62c-b318cfc4d0a0"

uuidAlreadyUsedEmail :: LB.ByteString
uuidAlreadyUsedEmail = "79a216d7-eaa9-4cd3-8f86-3535e0698fc0"

uuidInvalidEmail :: LB.ByteString
uuidInvalidEmail = "6633a7d9-9cd0-4453-9aba-dac6cf142d71"
