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


-- |
-- Module      : Massalia.RESTSchema
-- Description : A module to define schemas for REST API that can be verified
module Massalia.RESTSchema (
  Schema (..),
  ScalarSelect (..),
  ObjSelect (..),
  API (..),
  Person (..),
  PrivateData (..),
  db,
  adminAge,
  selection,
) where
import Control.Monad.Identity (IdentityT(IdentityT), Identity, (>=>))
import Data.Text (Text)
import GHC.Generics (Generic (Rep, to), M1 (..), D, C, S, Selector (selName), K1 (..), (:*:)(..))
import Massalia.SelectionTree (JsonMassaliaTree, MassaliaTree (lookupChildren, isLeaf))
import Data.Data (Proxy (..))
import Massalia.GenericUtils (proxySelName)
import qualified Data.Text as Text
import Control.Applicative (Applicative(liftA2))
import Massalia.Utils (intercalate)

class Schema a where
  parseQuery :: JsonMassaliaTree -> Either ParseQueryError (Selection a)
  default parseQuery :: (Generic (Selection a), GSchema (Rep (Selection a))) => JsonMassaliaTree -> Either ParseQueryError (Selection a)
  parseQuery = fmap to . gParseQuery Proxy

type Selection a = a ScalarSelect ObjSelect
type Value a = a IdentityT IdentityT

data ParseQueryError = PQError {
  pqe_ctx :: [Text],
  pqe_msg :: Text
}
instance Show ParseQueryError where
  show PQError{pqe_ctx, pqe_msg} = Text.unpack $
    "root" <> intercalate "." (reverse pqe_ctx) <> ": " <> pqe_msg
mkFieldErr :: [Text] -> Text -> Text -> ParseQueryError
mkFieldErr ctx field msg = PQError {
  pqe_msg = "'" <> field <> "'" <> " " <> msg,
  pqe_ctx = ctx
}

data ScalarSelect m a = SPick | SDiscard
  deriving (Generic, Functor, Show)

data ObjSelect m a = OPick a | ODiscard
  deriving (Generic, Functor, Show)

-- Generic code

class GSchema f where
  gParseQuery :: Proxy (f a) -> JsonMassaliaTree -> Either ParseQueryError (f a)

instance GSchema a => GSchema (M1 D s a) where
  gParseQuery _ = fmap M1 . gParseQuery (Proxy @(a _))
instance GSchema a => GSchema (M1 C s a) where
  gParseQuery _ = fmap M1 . gParseQuery (Proxy @(a _))
instance (Selector s) => GSchema (M1 S s (K1 i (ScalarSelect m a))) where
  gParseQuery _ t = M1 . K1 <$> case lookupChildren fieldName t of
      Nothing            -> Right SDiscard
      Just x | isLeaf x  -> Right SPick
             | otherwise -> Left (mkFieldErr [] fieldName "must be a leaf node")
    where fieldName = Text.pack $ selName (proxySelName :: (M1 S s _ _))
instance (Selector s, Schema a) => GSchema (M1 S s (K1 i (ObjSelect m (Selection a)))) where
  gParseQuery _ t = M1 . K1 <$> case lookupChildren fieldName t of
      Nothing            -> Right ODiscard
      Just x | isLeaf x  -> Left (mkFieldErr [] fieldName "cannot be a leaf node, must specify children")
             | otherwise -> OPick <$> parseQuery x
    where fieldName = Text.pack $ selName (proxySelName :: (M1 S s _ _))
instance (GSchema a, GSchema b) => GSchema (a :*: b) where
  gParseQuery _ t = liftA2 (:*:) (gParseQuery (Proxy @(a _)) t) (gParseQuery (Proxy @(b _)) t)


-- Examples

data API scalar obj = API {
  version :: scalar Identity Int,
  admin :: obj Identity (Person scalar obj),
  users :: obj [] (Person scalar obj)
} deriving (Generic)
instance Schema API

data Person scalar obj = Person {
  name :: scalar Identity Text,
  age :: scalar Maybe Int,
  favNumbers :: scalar [] Int,
  privateData :: obj Identity (PrivateData scalar obj)
} deriving (Generic)
instance Schema Person

newtype PrivateData scalar obj = PrivateData {
  password :: scalar Identity Text
} deriving (Generic)
instance Schema PrivateData

db :: Value API
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
adminAge :: Value API -> IdentityT _ Text
adminAge = admin >=> privateData >=> password

selection :: Selection API
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

deriving instance Show (Selection Person)
deriving instance Show (Selection API)
deriving instance Show (Selection PrivateData)