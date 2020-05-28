{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Massalia.MorpheusTypes
-- Description : A reexport of the handy functions and types from "Morpheus".
module Massalia.MorpheusTypes
  (
    module Data.Morpheus.Types
  )
where

import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson as JSON
import Data.Morpheus.Types
import qualified Data.Morpheus.Types as GQLT
import Massalia.Utils (
  EmailAddress, LocalTime, Text, UUID, Day,
  SimpleRange, Inclusivity (..),
  emailToByteString, emailValidate, stringToText, uuidFromText, uuidToText
  )
import Massalia.Auth (JWTEncodedString(JWTEncodedString))
import Data.Morpheus.Types (GQLType(description), KIND)
import Data.Morpheus.Kind (SCALAR, WRAPPER, INPUT)
import Protolude

instance GQLScalar UUID where
  parseValue (GQLT.String x) = case uuidFromText x of
    Nothing -> Left $ "The given value = " <> x <> " is not a correct UUID"
    Just iid -> pure iid
  serialize = GQLT.String . uuidToText

instance GQLType UUID where
  type KIND UUID = SCALAR

instance GQLScalar LocalTime where
  parseValue (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  serialize = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)

instance GQLType LocalTime where
  type KIND LocalTime = SCALAR

instance GQLScalar Day where
  parseValue (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  serialize = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)

instance GQLType Day where
  type KIND Day = SCALAR

instance GQLScalar EmailAddress where
  parseValue (GQLT.String x) = first stringToText $ emailValidate $ encodeUtf8 x
  serialize = GQLT.String . (decodeUtf8 . emailToByteString)

instance GQLType EmailAddress where
  type KIND EmailAddress = SCALAR

instance GQLScalar JWTEncodedString where
  parseValue (GQLT.String x) = pure $ JWTEncodedString x
  serialize (JWTEncodedString val) = GQLT.String val

instance GQLType JWTEncodedString where
  type KIND JWTEncodedString = SCALAR

instance GQLScalar Void where
  parseValue _ = Left "Impossible, trying to parse to void (that is, this field should be null or undefined)"
  serialize _ = panic "Impossible, trying to serialize void"

instance GQLType Void where
  type KIND Void = SCALAR
instance (Typeable a, GQLType a) => GQLType (SimpleRange a) where
  type KIND (SimpleRange a) = INPUT
  description = const $ Just ("A simple range structure for filtering in postgres, it's ultimately translated as a PostgresRange" :: Text)

instance GQLScalar Inclusivity where
  parseValue (GQLT.String x) = case x of
    "[]" -> Right II
    "[)" -> Right IE
    "(]" -> Right EI
    "()" -> Right EE
    _ -> Left $ "cannot parse range inclusivity = " <> x
  serialize x = case x of
    II -> GQLT.String "[]"
    IE -> GQLT.String "[)"
    EI -> GQLT.String "(]"
    EE -> GQLT.String "()"

instance GQLType Inclusivity where
  type KIND Inclusivity = SCALAR


-- instance GQLType PlantFilter where
--   type KIND PlantFilter = INPUT
--   description = const $ Just ("A set of filters for the Plant type" :: Text)
  

-- instance GQLScalar EmailAddress where
--   parseValue (GQLT.String x) = first stringToText $ emailValidate $ encodeUtf8 x
--   serialize = GQLT.String . (decodeUtf8 . emailToByteString)

-- instance GQLType EmailAddress where
--   type KIND EmailAddress = SCALAR

-- instance GQLScalar (Range a) where
--   parseValue (GQLT.String x) = first pack $ EmailAddress.validate $ encodeUtf8 x
--   serialize = GQLT.String . (decodeUtf8 . EmailAddress.toByteString)

-- instance GQLType EmailAddress where
--   type KIND EmailAddress = SCALAR
