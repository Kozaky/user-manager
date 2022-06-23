module Database.Connection (Connection (..), DbAuth (..)) where

import qualified Data.Text as T
import Database.MongoDB (Pipe)

data Connection = Connection Pipe T.Text

data DbAuth = DbAuth T.Text T.Text
