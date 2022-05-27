module DbConnection (DbConnection (..), DbAuth (..)) where

import qualified Data.Text as T
import Database.MongoDB (Pipe)

data DbConnection = DbConnection Pipe T.Text

data DbAuth = DbAuth T.Text T.Text
