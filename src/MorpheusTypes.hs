{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MorpheusTypes (
  Arguments,
  ValidArguments,
  Selection (..),
  SelectionContent (SelectionField, SelectionSet),
  SelectionSet,
  ValidSelection,
  ValidSelectionSet,
  ValidSelectionContent,
  Key,
  Position(Position),
  validSelectionField,
  Name,
  selectionGen,
  MergeSet(MergeSet),
  validSelectionToSelectionSet
) where

import Protolude
import Data.Morpheus.Types.Internal.AST.Selection
  (
    Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
  )
  
import Data.Morpheus.Types.Internal.AST.MergeSet (MergeSet(MergeSet))
import Data.Morpheus.Types.Internal.AST.Base (VALID, Key, Position(Position), Name)

import Data.Text (Text, unpack, pack)
import Data.Morpheus.Types (KIND, GQLType, GQLScalar(parseValue, serialize))
import qualified Data.Morpheus.Types as GQLT
import Data.Morpheus.Kind (SCALAR)

-- morpheus additional scalar definitions
import Data.UUID (nil, toText, fromText, UUID)
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress
import PostgreSQL.Binary.Data (LocalTime)
import Data.Aeson (eitherDecode)
import qualified Data.Aeson as JSON

type ValidSelection = Selection VALID
type ValidSelectionSet = SelectionSet VALID
type ValidArguments = Arguments VALID
type ValidSelectionContent = SelectionContent VALID

validSelectionField :: ValidSelectionContent 
validSelectionField = SelectionField

selectionGen :: Name -> ValidArguments -> ValidSelectionContent -> ValidSelection
selectionGen name a content = Selection {
    selectionName = name,
    selectionAlias = Nothing,
    selectionPosition = (Position 0 0),
    selectionArguments = a,
    selectionContent = content
  }

validSelectionToSelectionSet (Selection{ selectionContent = selection }) = case selection of
  SelectionField -> undefined -- error "graphql validation should prevent this, it should not exist"
  (SelectionSet deeperSel) -> deeperSel

-- GQL Scalar useful definitions

instance GQLScalar UUID where
  parseValue (GQLT.String x) = pure $ case fromText x of
    Nothing -> nil
    Just iid -> iid
  serialize = GQLT.String . toText

instance GQLType UUID where
  type KIND UUID = SCALAR

instance GQLScalar LocalTime where
  parseValue (GQLT.String x) = first pack $ eitherDecode $ JSON.encode x
  serialize = GQLT.String . (fromMaybe "" . JSON.decode . JSON.encode)

instance GQLType LocalTime where
  type KIND LocalTime = SCALAR

instance GQLScalar EmailAddress where
  parseValue (GQLT.String x) = first pack $ EmailAddress.validate $ encodeUtf8 x
  serialize = GQLT.String . (decodeUtf8 . EmailAddress.toByteString)

instance GQLType EmailAddress where
  type KIND EmailAddress = SCALAR

-- instance GQLScalar (Range a) where
--   parseValue (GQLT.String x) = first pack $ EmailAddress.validate $ encodeUtf8 x
--   serialize = GQLT.String . (decodeUtf8 . EmailAddress.toByteString)

-- instance GQLType EmailAddress where
--   type KIND EmailAddress = SCALAR