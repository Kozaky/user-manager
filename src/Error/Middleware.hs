module Error.Middleware (catchErrorMiddleware) where

import Data.Aeson (ToJSON (toJSON), Value (Object), encode)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LB
import Data.IORef (modifyIORef', newIORef, readIORef)
import Error.Constants (generalErrorMsg, uuidGeneralError)
import GHC.Exts (fromList)
import Network.HTTP.Types (Status (Status), hContentType)
import Network.Wai (Middleware, Response, responseLBS, responseStatus, responseToStream)

catchErrorMiddleware :: Middleware
catchErrorMiddleware baseApp req respond =
  baseApp req $ \response -> do
    let status = responseStatus response
    body <- responseBody response

    case status of
      (Status code _msg) | code >= 400 && code < 500 -> respond $ formatResponse code body
      _ -> respond response

responseBody :: Response -> IO B.ByteString
responseBody res = do
  let (_status, _headers, streamBody) = responseToStream res
  streamBody $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    LB.toStrict . toLazyByteString <$> readIORef content

formatResponse :: Int -> B.ByteString -> Response
formatResponse code body =
  responseLBS
    (Status code "")
    [(hContentType, "application/json")]
    $ formatBody body

formatBody :: B.ByteString -> LB.ByteString
formatBody body = do
  let parsedBody = parseBody body

  case parsedBody of
    ("", "") | body == "" -> mkBody (LB.toStrict uuidGeneralError) (LB.toStrict generalErrorMsg)
    ("", "") -> mkBody (LB.toStrict uuidGeneralError) body
    ("", errorMsg) -> mkBody (LB.toStrict uuidGeneralError) errorMsg
    (code, errorMsg) -> mkBody code errorMsg

mkBody :: B.ByteString -> B.ByteString -> LB.ByteString
mkBody code errorMsg =
  encode $
    Object $
      fromList
        [ ("code", toJSON . Char8.unpack . Char8.strip $ code),
          ("errorMsg", toJSON . Char8.unpack . Char8.strip $ errorMsg)
        ]

parseBody :: B.ByteString -> (B.ByteString, B.ByteString)
parseBody =
  B.splitAt 37
    . fst
    . B.breakSubstring "#error}"
    . B.drop 9
    . snd
    . B.breakSubstring "{#error -"
