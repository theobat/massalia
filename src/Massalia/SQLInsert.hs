{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.SQLInsert
-- Description : A module to ddefine an assembler function for creating insert into statements
module Massalia.SQLInsert
  ( valuesToInsertWrapped,
    valuesToInsert,
    valuesToSelect,
    ValuesFormatType (..),
    InsertSchema (..),
  )
where

import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Encoders as Encoders
import qualified Massalia.HasqlDec as Decoders
import Massalia.MorpheusTypes
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
    ValidSelection,
    ValidSelectionSet,
  )
import Massalia.QueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
    commaAssemble,
  )
import Massalia.Utils (toUnderscore)
import Protolude

-- | The type of values formatting.
data ValuesFormatType
  = -- | select * from (values (1), (2)) as t;
    WrapInSelect
  | -- | insert into (values (1), (2)) as t;
    PureValues

data QueryShape = InsertShape ValuesFormatType | SelectShape

-- | A simple triplet @(tableName, columnList, haskellType -> queryFormat)@
newtype InsertSchema queryFormat underlyingType = InsertSchema (queryFormat, [queryFormat], underlyingType -> queryFormat)

valuesToInsertWrapped,
  valuesToInsert,
  valuesToSelect ::
    (QueryFormat queryFormat, Foldable collection) =>
    -- | First is the tableName, then a column list and then the values encoder.
    InsertSchema queryFormat underlyingType ->
    -- | A collection of literal values to encode into the query.
    collection underlyingType ->
    -- | The end result for the insert query.
    queryFormat
valuesToInsertWrapped = valuesToQueryFormat (InsertShape WrapInSelect)
valuesToInsert = valuesToQueryFormat (InsertShape PureValues)
valuesToSelect = valuesToQueryFormat SelectShape

-- | A function to create insert statements out of haskell records.
valuesToQueryFormat ::
  (QueryFormat queryFormat, Foldable collection) =>
  -- | A parameter to specify how to wrap the literal values.
  QueryShape ->
  -- | First is the tableName, then a column list and then the values encoder.
  InsertSchema queryFormat underlyingType ->
  -- | A collection of literal values to encode into the query.
  collection underlyingType ->
  -- | The end result for the insert query.
  queryFormat
valuesToQueryFormat queryShape (InsertSchema (tableName, header, elementToQueryFormat)) collection = result
  where
    result = case queryShape of
      SelectShape -> partialRes WrapInSelect
      InsertShape format -> partialRes format
    partialRes formatType = valuesFormatter formatType columnListAssembled elementToQueryFormat collection
    headerRes = "INSERT INTO \"" <> tableName <> "\" " <> columnListAssembled
    columnListAssembled = "(" <> commaAssemble header <> ")"

-- | A function to form a simple select query out of a collection of input values.
valuesFormatter ::
  (QueryFormat queryFormat, Foldable collection) =>
  -- | A parameter to specify how to wrap the literal values. Default to 'WrapInSelect'.
  ValuesFormatType ->
  -- | A parameter to specify how to wrap the literal values.
  queryFormat ->
  -- | First is the tableName, then a column list and then the values encoder.
  (underlyingType -> queryFormat) ->
  -- | A collection of literal values to encode into the query.
  collection underlyingType ->
  -- | The end result for the insert query.
  queryFormat
valuesFormatter formatType columnListAssembled elementToQueryFormat collection = partialRes
  where
    partialRes = assembledRowsFormatter formatType assembledRows columnListAssembled
    rowSeparator a (0, previousRows) = rowSeparatorGeneric "\n" a (0, previousRows)
    rowSeparator a (index, previousRows) = rowSeparatorGeneric ",\n" a (index, previousRows)
    rowSeparatorGeneric sep a (index, previousRows) = (index + 1, previousRows <> sep <> "(" <> elementToQueryFormat a <> ")")
    (_, assembledRows) = foldr rowSeparator (0, "") collection

assembledRowsFormatter :: (QueryFormat queryFormat) => ValuesFormatType -> queryFormat -> queryFormat -> queryFormat
assembledRowsFormatter t assembledRows assembledColList = case t of
  WrapInSelect -> "SELECT * FROM (" <> coreContent <> ") as values_selection " <> assembledColList
  PureValues -> coreContent
  where
    coreContent = "VALUES " <> assembledRows
