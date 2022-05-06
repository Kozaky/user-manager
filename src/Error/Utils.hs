module Error.Utils (mkErrorMsg, customErrorPrefix, customErrorSufix) where

import qualified Data.ByteString.Lazy as LB

customErrorPrefix :: LB.ByteString
customErrorPrefix = "{error#"

customErrorSufix :: LB.ByteString
customErrorSufix = "#error}"

mkErrorMsg :: LB.ByteString -> LB.ByteString -> LB.ByteString
mkErrorMsg code errorMsg = customErrorPrefix <> code <> ":" <> errorMsg <> customErrorSufix
