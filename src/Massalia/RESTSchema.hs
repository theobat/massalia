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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      : Massalia.RESTSchema
-- Description : A module to define schemas for REST API that can be verified
module Massalia.RESTSchema (
) where
import Control.Monad.Identity (IdentityT(IdentityT), Identity, (>=>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Massalia.SelectionTree (JsonMassaliaTree)

class Schema a where
  parseQuery :: JsonMassaliaTree -> a ScalarSelect ObjSelect

data ScalarSelect m a = SPick | SDiscard
  deriving (Generic, Functor)

data ObjSelect m a = OPick a | ODiscard
  deriving (Generic, Functor)


-- Examples

data API scalar obj = API {
  version :: scalar Identity Int,
  admin :: obj Identity (Person scalar obj),
  users :: obj [] (Person scalar obj)
} deriving (Generic)

data Person scalar obj = Person {
  name :: scalar Identity Text,
  age :: scalar Maybe Int,
  favNumbers :: scalar [] Int,
  privateData :: obj Identity (PrivateData scalar obj)
} deriving (Generic)

newtype PrivateData scalar obj = PrivateData {
  password :: scalar Identity Text
} deriving (Generic)

db :: API IdentityT IdentityT
db = API {
  version = pure 20,
  admin = pure Person {
    name = pure "Diego",
    age = pure 20,
    favNumbers = IdentityT [-1, 2, 42, 64],
    privateData = pure PrivateData {
      password = pure "myPassword"
    }
  },
  users = IdentityT [
    -- Other users...
  ]
}

-- Transformers are easily composable
adminAge :: API _ _ -> IdentityT _ Text
adminAge = admin >=> privateData >=> password
 
selection :: API ScalarSelect ObjSelect
selection = API {
  version = SPick,
  admin = ODiscard,
  users = OPick Person {
    name = SPick,
    age = SDiscard,
    favNumbers = SPick,
    privateData = OPick PrivateData {
      password = SPick
    }
  }
}
