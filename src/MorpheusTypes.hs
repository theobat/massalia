{-# LANGUAGE TypeFamilies #-}

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
  safeFromList,
  Name,
  selectionGen,
  MergeSet(MergeSet),
  validSelectionToSelectionSet
) where

import Data.Morpheus.Types.Internal.AST.Selection
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
  )
  
import Data.Morpheus.Types.Internal.AST.MergeSet (MergeSet(MergeSet), safeFromList)
import Data.Morpheus.Types.Internal.AST.Base (VALID, Key, Position(Position), Name)

import Data.UUID (nil, toText, fromText, UUID)
import Data.Text (Text, unpack, pack)
import Data.Morpheus.Types (KIND, GQLType, GQLScalar(parseValue, serialize))
import qualified Data.Morpheus.Types as GQLT
import Data.Morpheus.Kind (SCALAR)

import PostgreSQL.Binary.Data (LocalTime)

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
  SelectionField -> error "graphql validation should prevent this, it should not exist"
  (SelectionSet deeperSel) -> deeperSel

instance GQLScalar UUID where
  parseValue (GQLT.String x) = pure $ case fromText x of
    Nothing -> nil
    Just iid -> iid
  serialize = GQLT.String . toText

instance GQLType UUID where
  type KIND UUID = SCALAR

instance GQLScalar LocalTime where
  parseValue (GQLT.String x) = undefined
  serialize = GQLT.String . (pack . show)

instance GQLType LocalTime where
  type KIND LocalTime = SCALAR