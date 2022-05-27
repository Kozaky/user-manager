module Types.User (UserDTO (..), CreateUserReq, userDTOfromUser, userFromCreateUserReq, mkEmail, Email(), pattern Email, EditUserReq, Request (..)) where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
    withText,
  )
import Data.Aeson.Encoding (text)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Error.Types (ApiError (toBody), UserError (InvalidEmailError))
import GHC.Generics (Generic)
import Model.User (User (..))

data UserDTO = UserDTO
  { id :: !T.Text,
    name :: !T.Text,
    email :: !T.Text
  }
  deriving (Show, Generic)

instance FromJSON UserDTO

instance ToJSON UserDTO where
  toEncoding = genericToEncoding defaultOptions

userDTOfromUser :: User -> UserDTO
userDTOfromUser (User userId userName userEmail _) =
  UserDTO
    { id = fromMaybe "" userId,
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

instance ToJSON CreateUserReq where
  toEncoding = genericToEncoding defaultOptions

type EditUserReq = Request Maybe

deriving instance Show EditUserReq

instance FromJSON EditUserReq

instance ToJSON EditUserReq where
  toEncoding = genericToEncoding defaultOptions

userFromCreateUserReq :: CreateUserReq -> User
userFromCreateUserReq (Request name (Identity (UnsafeEmail email)) password) =
  User
    { id = Nothing,
      name = runIdentity name,
      email = runIdentity (Identity email),
      password = runIdentity password
    }

newtype Email = UnsafeEmail T.Text deriving (Show, Generic)

pattern Email :: T.Text -> Email
pattern Email a <- UnsafeEmail a

mkEmail :: T.Text -> Maybe Email
mkEmail emailText
  | T.any (== '@') emailText = Just $ UnsafeEmail emailText
  | otherwise = Nothing

instance FromJSON Email where
  parseJSON = withText "email" $ \emailText -> do
    let mEmail = mkEmail emailText
    case mEmail of
      Just email -> return email
      Nothing -> fail . B.unpack . toBody $ InvalidEmailError 

instance ToJSON Email where
  toEncoding (UnsafeEmail email) = text email
