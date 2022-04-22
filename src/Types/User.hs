module Types.User (UserDTO (..), CreateUserReq, userDTOfromUser, userFromCreateUserReq, mkEmail, Email (unEmail), EditUserReq, Request (..)) where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
    withText,
  )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor.Identity (Identity, runIdentity)
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

type EditUserReq = Request Maybe

deriving instance Show EditUserReq

instance FromJSON EditUserReq

userFromCreateUserReq :: CreateUserReq -> User
userFromCreateUserReq (Request name email password) =
  User
    { id = Nothing,
      name = runIdentity name,
      email = unEmail $ runIdentity email,
      password = runIdentity password
    }

newtype Email = Email {unEmail :: T.Text} deriving (Show)

mkEmail :: T.Text -> Either B.ByteString Email
mkEmail text
  | T.any (== '@') text = Right $ Email text
  | otherwise = Left $ toBody InvalidEmailError

instance FromJSON Email where
  parseJSON = withText "email" $ \text -> do
    let mEmail = mkEmail text
    case mEmail of
      Right email -> return email
      Left msg -> fail $ B.unpack msg
