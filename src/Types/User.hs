module Types.User (UserDTO (..), CreateUserReq, userDTOfromUser, userFromCreateUserReq, mkEmail, Email, EditUserReq, Request (..), emailAsText) where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
    withText,
  )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T
import Database.Persist.MongoDB (keyToOid)
import Error.Constants (uuidInvalidEmail)
import Error.Utils (mkErrorMsg)
import GHC.Generics (Generic)
import Model.User (Key, User (User, userEmail, userName, userPassword))

data UserDTO = UserDTO
  { id :: !T.Text,
    name :: !T.Text,
    email :: !T.Text
  }
  deriving (Show, Generic)

instance ToJSON UserDTO where
  toEncoding = genericToEncoding defaultOptions

userDTOfromUser :: (Key User, User) -> UserDTO
userDTOfromUser (userId, User {userName, userEmail}) =
  UserDTO
    { id = T.pack . show . keyToOid $ userId,
      name = userName,
      email = userEmail
    }

data Request f = Request
  { name :: !(f T.Text),
    email :: !(f Email),
    password :: !(f T.Text)
  }
  deriving (Generic)

type CreateUserReq = Request Identity

deriving instance Show CreateUserReq

instance FromJSON CreateUserReq

type EditUserReq = Request Maybe

deriving instance Show EditUserReq

instance FromJSON EditUserReq

userFromCreateUserReq :: CreateUserReq -> User
userFromCreateUserReq (Request name email password) =
  User
    { userName = runIdentity name,
      userEmail = unEmail $ runIdentity email,
      userPassword = runIdentity password
    }

newtype Email = Email {unEmail :: T.Text} deriving (Show)

emailAsText :: Email -> T.Text
emailAsText = unEmail

mkEmail :: T.Text -> Either B.ByteString Email
mkEmail text
  | T.any (== '@') text = Right $ Email text
  | otherwise = Left $ mkErrorMsg uuidInvalidEmail "Invalid Email"

instance FromJSON Email where
  parseJSON = withText "email" $ \text -> do
    let mEmail = mkEmail text
    case mEmail of
      Right email -> return email
      Left msg -> fail $ B.unpack msg
