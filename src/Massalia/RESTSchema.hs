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
  Person (..), API (..),
  restSchemaValidate
) where
import Data.Text (Text)
import Massalia.Utils (intercalate)
import Protolude (Generic (Rep), Proxy (..), (:*:), M1, K1, Selector (selName), Alternative ((<|>)))
import Massalia.SelectionTree (JsonMassaliaTree, MassaliaTree (getName, getChildrenList), isLeaf)
import qualified Data.Text as Text
import GHC.Generics (S, D, C)

data RestSchemaField = RestSchemaLeaf | forall b. RestSchema b => RestSchemaObject (Proxy b)

class RestSchema a where
  restSchemaLookupField :: Proxy a -> Text -> Maybe RestSchemaField
  default restSchemaLookupField :: (Generic a, GRestSchema (Rep a)) => Proxy a -> Text -> Maybe RestSchemaField
  restSchemaLookupField (_ :: Proxy a) = gRestSchemaLookupField (Proxy @(Rep a _))

class GRestSchema f where
  gRestSchemaLookupField :: Proxy (f a) -> Text -> Maybe RestSchemaField
class GToRestSchemaField f where
  gToRestSchemaField :: Proxy (f a) -> RestSchemaField

instance (GRestSchema a, GRestSchema b) => GRestSchema (a :*: b) where
  gRestSchemaLookupField (_ :: Proxy ((:*:) a b p)) name =
    gRestSchemaLookupField (Proxy @(a p)) name <|> gRestSchemaLookupField (Proxy @(b p)) name
instance (GToRestSchemaField a, Selector s) => GRestSchema (M1 S s a) where
  gRestSchemaLookupField (_ :: Proxy (M1 S s a p)) name
    | fieldName == name = Just (gToRestSchemaField (Proxy @(a p)))
    | otherwise = Nothing
    where fieldName = Text.pack $ selName (undefined :: M1 S s _ p)
instance (GRestSchema a) => GRestSchema (M1 D s a) where
  gRestSchemaLookupField (_ :: Proxy (M1 D s a p)) = gRestSchemaLookupField (Proxy @(a p))
instance (GRestSchema a) => GRestSchema (M1 C s a) where
  gRestSchemaLookupField (_ :: Proxy (M1 C s a p)) = gRestSchemaLookupField (Proxy @(a p))

instance (GToRestSchemaField a) => GToRestSchemaField (M1 i s a) where
  gToRestSchemaField (_ :: Proxy (M1 i s a p)) = gToRestSchemaField (Proxy @(a p))
instance {-# OVERLAPPABLE #-} (RestSchema a) => GToRestSchemaField (K1 i a) where
  gToRestSchemaField (_ :: Proxy (K1 i a p)) = RestSchemaObject (Proxy @a)
instance GToRestSchemaField (K1 i Int) where
  gToRestSchemaField _ = RestSchemaLeaf
instance GToRestSchemaField (K1 i String) where
  gToRestSchemaField _ = RestSchemaLeaf
instance GToRestSchemaField (K1 i Text) where
  gToRestSchemaField _ = RestSchemaLeaf


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
