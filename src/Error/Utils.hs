module Error.Utils (mkErrorMsg) where

import qualified Data.ByteString.Lazy as LB

mkErrorMsg :: LB.ByteString -> LB.ByteString -> LB.ByteString
mkErrorMsg code errorMsg = "{#error -" <> code <> ":" <> errorMsg <> "#error}"
