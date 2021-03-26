{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Massalia.MorpheusTypes
-- Description : A reexport of the handy functions and types from "Morpheus".
-- And their orphan instances for GQLType
module Massalia.MorpheusTypes
  (
    module Data.Morpheus.Types,
    module Data.Morpheus.Types.GQLScalar
  )
where

import qualified Data.Aeson as JSON
import Data.Morpheus.Types.GQLScalar ( EncodeScalar(encodeScalar), DecodeScalar(decodeScalar) )
import Data.Morpheus.Types (GQLType(description), KIND)
import Data.Morpheus.Kind (SCALAR)
import qualified Data.Morpheus.Types as GQLT
import Massalia.Utils (UTCTime, 
  EmailAddress, LocalTime, ZonedTime, ZonedTimeEq, UUID, Day,
  SimpleRange, Inclusivity (..),
  emailToByteString, emailValidate, stringToText, uuidFromText, uuidToText
  )
import Massalia.Auth (JWTEncodedString(JWTEncodedString))
import Protolude

instance EncodeScalar UUID where
  encodeScalar = GQLT.String . uuidToText
instance DecodeScalar UUID where
  decodeScalar (GQLT.String x) = case uuidFromText x of
    Nothing -> Left $ "The given value = " <> x <> " is not a correct UUID"
    Just iid -> pure iid
  decodeScalar _ = Left "The given value is not a string"

instance GQLType UUID where
  type KIND UUID = SCALAR

instance EncodeScalar UTCTime where
  encodeScalar = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)
instance DecodeScalar UTCTime where
  decodeScalar (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  decodeScalar _ = Left "UTCTime can only be String"

instance GQLType UTCTime where
  type KIND UTCTime = SCALAR

instance EncodeScalar LocalTime where
  encodeScalar = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)
instance DecodeScalar LocalTime where
  decodeScalar (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  decodeScalar _ = Left "LocalTime can only be String"

instance GQLType LocalTime where
  type KIND LocalTime = SCALAR

instance EncodeScalar ZonedTime where
  encodeScalar = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)
instance DecodeScalar ZonedTime where
  decodeScalar (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  decodeScalar _ = Left "ZonedTime can only be String"

instance GQLType ZonedTime where
  type KIND ZonedTime = SCALAR

instance EncodeScalar ZonedTimeEq where
  encodeScalar = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)
instance DecodeScalar ZonedTimeEq where
  decodeScalar (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  decodeScalar _ = Left "ZonedTimeEq can only be String"

instance GQLType ZonedTimeEq where
  type KIND ZonedTimeEq = SCALAR


instance EncodeScalar Day where
  encodeScalar = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)
instance DecodeScalar Day where
  decodeScalar (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  decodeScalar _ = Left "Day can only be String"

instance GQLType Day where
  type KIND Day = SCALAR

instance EncodeScalar EmailAddress where
  encodeScalar = GQLT.String . (decodeUtf8 . emailToByteString)
instance DecodeScalar EmailAddress where
  decodeScalar (GQLT.String x) = first stringToText $ emailValidate $ encodeUtf8 x
  decodeScalar _ = Left "EmailAddress can only be String"

instance GQLType EmailAddress where
  type KIND EmailAddress = SCALAR

instance EncodeScalar JWTEncodedString where
  encodeScalar (JWTEncodedString val) = GQLT.String val
instance DecodeScalar JWTEncodedString where
  decodeScalar (GQLT.String x) = pure $ JWTEncodedString x
  decodeScalar _ = Left "JWTEncodedString can only be String"

instance GQLType JWTEncodedString where
  type KIND JWTEncodedString = SCALAR

instance EncodeScalar Void where
  encodeScalar _ = panic "Impossible, trying to serialize void"
instance DecodeScalar Void where
  decodeScalar _ = Left "Impossible, trying to parse to void (that is, this field should be null or undefined)"

instance GQLType Void where
  type KIND Void = SCALAR

instance (Typeable a, GQLType a) => GQLType (SimpleRange a) where
  description = const $ Just ("A simple range structure for filtering in postgres, it's ultimately translated as a PostgresRange" :: Text)

instance EncodeScalar Inclusivity where
  encodeScalar x = case x of
    II -> GQLT.String "[]"
    IE -> GQLT.String "[)"
    EI -> GQLT.String "(]"
    EE -> GQLT.String "()"
instance DecodeScalar Inclusivity where
  decodeScalar (GQLT.String x) = case x of
    "[]" -> Right II
    "[)" -> Right IE
    "(]" -> Right EI
    "()" -> Right EE
    _ -> Left $ "cannot parse range inclusivity = " <> x
  decodeScalar _ = Left "Inclusivity can only be a String"

instance GQLType Inclusivity where
  type KIND Inclusivity = SCALAR



