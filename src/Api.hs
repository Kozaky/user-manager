module API where

import User.UserAPI (UserAPI)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API
  ( Get,
    PlainText,
    type (:<|>),
    type (:>),
  )

type API =
  UserAPI
    :<|> "routes" :> Get '[PlainText] Text

api :: Proxy API
api = Proxy
