module Model.User where

import Data.Text (Text)
import Database.Persist.MongoDB (MongoContext)
import Database.Persist.TH
  ( mkPersist,
    mkPersistSettings,
    persistLowerCase,
    share,
  )
import Language.Haskell.TH.Syntax (Type (ConT))

let mongoSettings =
      mkPersistSettings (ConT ''MongoContext)
 in share
      [mkPersist mongoSettings]
      [persistLowerCase|
        User
          name Text
          email Text
          password Text
          deriving Eq Read Show
      |]
