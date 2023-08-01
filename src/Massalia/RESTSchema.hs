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
{-# HLINT ignore "Use newtype instead of data" #-}
-- |
-- Module      : Massalia.RESTSchema
-- Description : A module to define schemas for REST API that can be verified
module Massalia.RESTSchema (
) where
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Massalia.SelectionTree (JsonMassaliaTree)

class Schema (a :: (Type -> Type) -> (Type -> Type) -> Type) where
  parseQuery :: JsonMassaliaTree -> a ScalarSelect ObjSelect
  default parseQuery :: Generic (a ScalarSelect ObjSelect) => JsonMassaliaTree -> a ScalarSelect ObjSelect
  parseQuery = undefined

data ScalarSelect a = SSPick | SSDiscard
data ObjSelect a where
  SSSelect :: a ScalarSelect ObjSelect -> ObjSelect (rel (a ScalarSelect ObjSelect))

data Person scalar obj = Person {
  name :: scalar Text,
  age :: scalar Int,
  favNumbers :: scalar [Int]
} deriving (Generic)

data API scalar obj = API {
  version :: scalar Int,
  admin :: obj (Identity (Person scalar obj)),
  users :: obj [Person scalar obj]
} deriving (Generic)

selection :: API ScalarSelect ObjSelect
selection = API {
  version = SSPick,
  admin = SSSelect Person {
    name = SSPick,
    age = SSDiscard,
    favNumbers = SSPick
  },
  users = SSSelect Person {
    name = SSPick,
    age = SSDiscard,
    favNumbers = SSDiscard
  }
}