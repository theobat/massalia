{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.MorpheusTypes
-- Description : A reexport of the handy functions and types from "Morpheus".
module Massalia.MorpheusTypes
  ( Arguments,
    ValidArguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
    ValidSelection,
    ValidSelectionSet,
    ValidSelectionContent,
    Key,
    Position (Position),
    validSelectionField,
    Name,
    selectionGen,
    MergeSet (MergeSet),
    validSelectionToSelectionSet,
    module Data.Morpheus.Types
  )
where

import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson as JSON
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
import qualified Data.Morpheus.Types as GQLT
import Data.Morpheus.Types.Internal.AST.Base (Key, Name, Position (Position), VALID)
import Data.Morpheus.Types.Internal.AST.MergeSet (MergeSet (MergeSet))
import Data.Morpheus.Types.Internal.AST.Selection
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
  )
import Massalia.Utils (EmailAddress, LocalTime, Text, UUID, emailToByteString, emailValidate, stringToText, uuidFromText, uuidToText)
import Massalia.Auth (JWTEncodedString(JWTEncodedString))
import Protolude

type ValidSelection = Selection VALID

type ValidSelectionSet = SelectionSet VALID

type ValidArguments = Arguments VALID

type ValidSelectionContent = SelectionContent VALID

validSelectionField :: ValidSelectionContent
validSelectionField = SelectionField

selectionGen :: Name -> ValidArguments -> ValidSelectionContent -> ValidSelection
selectionGen name a content =
  Selection
    { selectionName = name,
      selectionAlias = Nothing,
      selectionPosition = (Position 0 0),
      selectionArguments = a,
      selectionContent = content
    }

validSelectionToSelectionSet (Selection {selectionContent = selection}) = case selection of
  SelectionField -> undefined -- error "graphql validation should prevent this, it should not exist"
  (SelectionSet deeperSel) -> deeperSel

-- GQL Scalar useful definitions

instance GQLScalar UUID where
  parseValue (GQLT.String x) = case uuidFromText x of
    Nothing -> undefined -- pure ("The given value = " <> x <> " is not a correct UUID")
    Just iid -> pure iid
  serialize = GQLT.String . uuidToText

instance GQLType UUID where
  type KIND UUID = SCALAR

instance GQLScalar LocalTime where
  parseValue (GQLT.String x) = first stringToText $ JSON.eitherDecode $ JSON.encode x
  serialize = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)

instance GQLType LocalTime where
  type KIND LocalTime = SCALAR

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
