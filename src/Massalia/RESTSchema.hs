{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}


{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |
-- Module      : Massalia.RESTSchema
-- Description : A module to define schemas for REST API that can be verified
module Massalia.RESTSchema (
) where
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import Data.Kind (Type)

class Schema (f :: (Type -> Type) -> Type) where
  getSchema :: f SchemaField

data SchemaField a where
  SFLeaf :: SchemaField (Identity a)
  SFObject :: Schema a => (a SchemaField) -> SchemaField (Identity (a SchemaField))
  SFList :: SchemaField (Identity a) -> SchemaField [a]

data SchemaSelection a where
  SSFalse :: SchemaSelection (m a)
  SSTrue :: SchemaSelection (m a)
  SSSelectFields :: Schema a => a SchemaSelection -> SchemaSelection (m (a SchemaSelection))

-- EXAMPLE

data Person f = Person {
  name :: f (Identity Text),
  age :: f (Identity Int),
  email :: f (Identity Text),
  favoriteNumbers :: f [Int]
}

data API f = API {
  version :: f (Identity Int),
  webMaster :: f (Identity (Person f)),
  users :: f [Person f]
}

instance Schema API where
  getSchema = API {
    version = SFLeaf,
    webMaster = SFObject getSchema,
    users = SFList (SFObject getSchema)
  }

instance Schema Person where
  getSchema = Person {
    name = SFLeaf,
    age = SFLeaf,
    email = SFLeaf,
    favoriteNumbers = SFList SFLeaf
  }


selection :: API SchemaSelection
selection = API {
  version = SSFalse,
  webMaster = SSTrue,
  users = SSSelectFields Person {
    name = SSTrue,
    age = SSFalse,
    email = SSTrue,
    favoriteNumbers = SSTrue
  }
}