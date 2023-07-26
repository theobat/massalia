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

-- |
-- Module      : Massalia.RESTSchema
-- Description : A module to define schemas for REST API that can be verified
module Massalia.RESTSchema (
  RestSchema (..), RestSchemaField (..),
  RestSchemaValidateContext, RestSchemaValidateError (..),
  Person (..), API (..),
  restSchemaValidate
) where
import Data.Text (Text)
import Massalia.Utils (intercalate)
import Protolude (Generic, Proxy (..))
import Massalia.SelectionTree (JsonMassaliaTree, MassaliaTree (getName, getChildrenList), isLeaf)
import qualified Data.Text as Text

data RestSchemaField = RestSchemaLeaf | forall b. RestSchema b => RestSchemaObject (Proxy b)

class RestSchema a where
  restSchemaLookupField :: Proxy a -> Text -> Maybe RestSchemaField

restSchemaValidate :: RestSchema a =>
  Proxy a -> RestSchemaValidateContext -> JsonMassaliaTree -> [RestSchemaValidateError]
restSchemaValidate (Proxy :: Proxy a) oldCtx input =
  case restSchemaLookupField (Proxy @a) (getName input) of
    Nothing                                  -> [mkErr oldCtx (getName input) "does not exist"]
    Just (RestSchemaObject s) | isLeaf input -> [mkErr ctx (getName input) "has no fields specified"]
                              | otherwise    -> getChildrenList input >>= restSchemaValidate s ctx
    Just RestSchemaLeaf | isLeaf input       -> []
                        | otherwise          -> [mkErr ctx (getName input) "must be a leaf node"]
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
instance RestSchema Person where
  restSchemaLookupField _ "name" = Just RestSchemaLeaf
  restSchemaLookupField _ "age" = Just RestSchemaLeaf
  restSchemaLookupField _ _ = Nothing

data API = API {
  person :: Person,
  users :: Int
}
instance RestSchema API where
  restSchemaLookupField _ "person" = Just (RestSchemaObject (Proxy @Person))
  restSchemaLookupField _ "users" = Just RestSchemaLeaf
  restSchemaLookupField _ _ = Nothing


mkErr :: RestSchemaValidateContext -> Text -> Text -> RestSchemaValidateError
mkErr ctx fieldName message = RestSchemaValidateError{
  vrs_errorMessage=message,
  vrs_errorCtx=ctx,
  vrs_errorFieldName=fieldName
}
formatCtx :: RestSchemaValidateContext -> Text
formatCtx = intercalate "." . reverse
