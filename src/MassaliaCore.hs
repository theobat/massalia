{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaCore
  ( MassaliaStruct (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Selection
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
    ValidSelection,
    ValidSelectionSet,
  )
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

type ReUpdater recordType fieldType = (recordType -> fieldType -> recordType)

type ReEUpdater recordType fieldType wrapper = (recordType -> wrapper fieldType -> recordType)

type ReGetter recordType fieldType = (recordType -> fieldType)

class MassaliaStruct wrapper someType recordType where
  getInitialValue :: (Text, Text) -> recordType -> wrapper someType recordType
  simpleCol ::
    Show fieldType =>
    Text ->
    ReUpdater recordType fieldType ->
    ReGetter recordType fieldType ->
    Decoders.Value fieldType ->
    wrapper someType recordType ->
    wrapper someType recordType
  exprCol ::
    Show fieldType =>
    (Text, Text) ->
    ReUpdater recordType fieldType ->
    ReGetter recordType fieldType ->
    Decoders.Value fieldType ->
    wrapper someType recordType ->
    wrapper someType recordType
  subColList ::
    Monoid (wrapInA nestedRecordType) =>
    (Decoders.NullableOrNot Decoders.Value nestedRecordType -> Decoders.Value (wrapInA nestedRecordType)) ->
    (ValidSelectionSet -> wrapper someType nestedRecordType) ->
    (Text, Text) ->
    ReEUpdater recordType nestedRecordType wrapInA ->
    ValidSelection ->
    wrapper someType recordType ->
    wrapper someType recordType

-- class MassaliaFilter someType recordType where
--   impactFilter ::
--     MassaliaStruct wrapper someType recordType =>
--     someType ->
--     wrapper someType recordType ->
--     wrapper someType recordType
