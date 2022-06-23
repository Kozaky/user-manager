module User.User (User (..)) where

import Data.Bson (ObjectId, (=:))
import qualified Data.Bson as Bson
import qualified Data.Text as T
import Database.MongoDBService (Documentable (fromDocument, toDocument))

data User = User {id :: Maybe T.Text, name :: T.Text, email :: T.Text, password :: T.Text}
  deriving (Eq, Read, Show)

instance Documentable User where
  fromDocument doc = do
    let userId :: ObjectId = Bson.at "_id" doc
        name = Bson.at "name" doc
        email = Bson.at "email" doc
        password = Bson.at "password" doc

    User (Just . T.pack . show $ userId) name email password

  toDocument (User _ name email password) =
    [ "name" =: name,
      "email" =: email,
      "password" =: password
    ]
