module Error.Middleware (catchErrorMiddleware) where

import Data.Aeson (ToJSON (toJSON), Value (Object), encode)
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LB
import Data.Char qualified as Char
import Data.IORef (modifyIORef', newIORef, readIORef)
import Error.Utils (customErrorPrefix, customErrorSufix)
import GHC.Exts (fromList)
import Network.HTTP.Types (Status (Status), hContentType)
import Network.Wai (Middleware, Response, responseLBS, responseStatus, responseToStream)
import Error.Types (ApiError(key, msg), CustomServerError (InternalServerError))

catchErrorMiddleware :: Middleware
catchErrorMiddleware baseApp req respond =
  baseApp req $ \response -> do
    let status = responseStatus response
    body <- responseBody response

    case status of
      (Status 404 _) | body == "" -> respond $ formatResponse 404 "Not Found"
      (Status code _) | code >= 400 && code <= 500 -> respond $ formatResponse code body
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
    ("", "") | body == "" -> makeBody (LB.toStrict $ key InternalServerError) (LB.toStrict $ msg InternalServerError)
    ("", "") -> makeBody (LB.toStrict $ key InternalServerError) body
    ("", errorMsg) -> makeBody (LB.toStrict $ key InternalServerError) errorMsg
    (code, errorMsg) -> makeBody code errorMsg

makeBody :: B.ByteString -> B.ByteString -> LB.ByteString
makeBody code errorMsg =
  encode $
    Object $
      fromList
        [
          ("code", toJSON . Char8.unpack . Char8.strip $ code),
          ("errorMsg", toJSON . Char8.unpack . Char8.strip $ errorMsg)
        ]

parseBody :: B.ByteString -> (B.ByteString, B.ByteString)
parseBody =
  second (B.drop 1)
    . B.break ((==) $ fromIntegral $ Char.ord ':')
    . fst
    . B.breakSubstring (LB.toStrict customErrorSufix)
    . B.drop (fromIntegral $ LB.length customErrorPrefix)
    . snd
    . B.breakSubstring (LB.toStrict customErrorPrefix)
