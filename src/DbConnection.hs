module DbConnection where

import qualified Data.Text as T
import Database.MongoDB (Pipe)

data DbConnection = DbConnection {pipe :: Pipe, dbName :: T.Text}

data DbAuth = DbAuth {user :: T.Text, password :: T.Text}
