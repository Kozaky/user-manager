module API where

import API.User (UserAPI)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API
  ( Get,
    PlainText,
    type (:<|>),
    type (:>),
  )

type Api =
  UserAPI
    :<|> "routes" :> Get '[PlainText] Text

api :: Proxy Api
api = Proxy
