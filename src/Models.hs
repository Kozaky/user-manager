{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
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
          age  Int
          UniqueName name
          deriving Eq Read Show
      |]

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User <$> v .: "name"
      <*> v .: "age"

instance ToJSON User where
  toJSON (User name age) =
    object
      [ "name" .= name,
        "age" .= age
      ]
