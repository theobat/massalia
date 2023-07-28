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
  RestSchema (..), RestSchemaField (..),
  RestSchemaValidateContext, RestSchemaValidateError (..),
  Person (..), API (..), GenericIsObject (..),
  restSchemaValidate
) where
import Data.Text (Text)
import Massalia.Utils (intercalate)
import Protolude (Generic (Rep, to, from), Proxy (..), (:*:), M1, K1, Selector (selName), Alternative ((<|>)), Type, U1, Constructor)
import Massalia.SelectionTree (JsonMassaliaTree, MassaliaTree (getName, getChildrenList), isLeaf)
import qualified Data.Text as Text
import GHC.Generics (S, D, C, R, (:+:), Constructor (conIsRecord))
import Massalia.GenericUtils (proxySelName)

data RestSchemaField = RestSchemaLeaf | forall b. RestSchema b => RestSchemaObject (Proxy b)

class RestSchema a where
  restSchemaLookupField :: Proxy a -> Text -> Maybe RestSchemaField
  default restSchemaLookupField :: (Generic a, GRestSchema (Rep a)) => Proxy a -> Text -> Maybe RestSchemaField
  restSchemaLookupField (_ :: Proxy a) = gRestSchemaLookupField (Proxy @(Rep a))

class GRestSchema (f :: Type -> Type) where
  gRestSchemaLookupField :: Proxy f -> Text -> Maybe RestSchemaField

instance (GRestSchema a, GRestSchema b) => GRestSchema (a :*: b) where
  gRestSchemaLookupField _ name =
    gRestSchemaLookupField (Proxy @a) name <|> gRestSchemaLookupField (Proxy @b) name
instance (GRestSchema a) => GRestSchema (M1 D s a) where
  gRestSchemaLookupField _ = gRestSchemaLookupField (Proxy @a)
instance (GRestSchema a) => GRestSchema (M1 C s a) where
  gRestSchemaLookupField _ = gRestSchemaLookupField (Proxy @a)
instance (Selector s) => GRestSchema (M1 S s (K1 R a)) where
  gRestSchemaLookupField _ name
    | fieldName == name = undefined
    | otherwise = Nothing
    where fieldName = Text.pack $ selName (proxySelName :: M1 S s _ p)

class GenericIsObject (f :: Type -> Type) where
  gIsObject :: Proxy f -> Bool

instance (GenericIsObject a) => GenericIsObject (M1 D s a) where
  gIsObject _ = gIsObject (Proxy @a)
instance (Constructor s) => GenericIsObject (M1 C s a) where
  gIsObject _ = conIsRecord (proxySelName :: M1 C s a p)
instance GenericIsObject (a :+: b) where
  gIsObject _ = False


restSchemaValidate :: RestSchema a =>
  Proxy a -> RestSchemaValidateContext -> JsonMassaliaTree -> [RestSchemaValidateError]
restSchemaValidate (Proxy :: Proxy a) oldCtx input =
  case restSchemaLookupField (Proxy @a) (getName input) of
    Nothing                                -> [mkErr oldCtx (getName input) "does not exist"]
    Just RestSchemaLeaf | isLeaf input     -> []
                        | otherwise        -> [mkErr ctx (getName input) "must be a leaf node"]
    Just (RestSchemaObject s)
      | not . null $ getChildrenList input -> getChildrenList input >>= restSchemaValidate s ctx
      | otherwise                          -> [mkErr ctx (getName input) "has no fields specified"]
  where
    ctx = getName input : oldCtx

-- Each new context is added at the head of the list
type RestSchemaValidateContext = [Text]

data RestSchemaValidateError = RestSchemaValidateError {
  vrs_errorMessage :: Text,
  vrs_errorCtx :: RestSchemaValidateContext,
  vrs_errorFieldName :: Text
} deriving (Eq)
instance Show RestSchemaValidateError where
  show e = Text.unpack $
    formatCtx (vrs_errorCtx e) <> ": '" <> vrs_errorFieldName e <> "' " <> vrs_errorMessage e

data Person = Person { name :: Text, age :: Int }
  deriving (Show, Eq, Generic)
instance RestSchema Person

data API = API {
  person :: Person,
  users :: Int
} deriving (Show, Eq, Generic)
instance RestSchema API

mkErr :: RestSchemaValidateContext -> Text -> Text -> RestSchemaValidateError
mkErr ctx fieldName message = RestSchemaValidateError{
  vrs_errorMessage=message,
  vrs_errorCtx=ctx,
  vrs_errorFieldName=fieldName
}
formatCtx :: RestSchemaValidateContext -> Text
formatCtx = intercalate "." . reverse
