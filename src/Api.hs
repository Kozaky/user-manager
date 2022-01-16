{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Database.Persist (PersistEntity (Key))
import Models (User)
import Servant.API
  ( Capture,
    Get,
    JSON,
    Post,
    ReqBody,
    type (:<|>),
    type (:>),
  )

type Api =
  "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
    :<|> "user" :> Capture "name" Text :> Get '[JSON] (Maybe User)
    :<|> "hola" :> Get '[JSON] (Maybe String)

api :: Proxy Api
api = Proxy
