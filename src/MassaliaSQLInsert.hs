{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MassaliaSQLInsert
  ( inputToInsertStatement,
    InsertType(..)
  )
where

import Data.Maybe (fromMaybe)
import MorpheusTypes
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
    ValidSelection,
    ValidSelectionSet,
  )
import Data.String (IsString (..))
import Data.Text (Text)
import MassaliaUtils (commaAssemble)
import Protolude
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Encoders as Encoders
import Text.Inflections (toUnderscore)
import MassaliaQueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
  )
import MassaliaSQLPart (
    AQueryPart(AQueryPartConst),
    getContent,
    getListContent,
    getMaybeContent,
    AssemblingOptions(..),
    defaultAssemblingOptions,
    testAssemblingOptions
  )

data InsertType =
  -- | select * from (values (1), (2)) as t;
  WrapInSelect |
  -- | insert into (values (1), (2)) as t;
  PureValues

-- | A function to create insert statements out of haskell records.
--
inputToInsertStatement ::
  (QueryFormat queryFormat, Foldable collection) =>
  -- | A parameter to specify how to wrap the literal values.
  Maybe InsertType ->
  -- | First is the tableName, then a column list and then the values encoder.
  (queryFormat, [queryFormat], a -> queryFormat) ->
  -- | A collection of literal values to encode into the query.
  collection a ->
  -- | The end result for the insert query.
  queryFormat
inputToInsertStatement maybeInsertType (tableName, header, elementToQueryFormat) collection = result 
  where
    result = headerRes <> "\n" <> partialRes
    partialRes = assembledRowsFormatter (fromMaybe WrapInSelect maybeInsertType) assembledRows columnListAssembled
    headerRes = "INSERT INTO \"" <> tableName <> "\" " <> columnListAssembled
    columnListAssembled = "(" <> commaAssemble header <> ")"
    rowSeparator a (0, previousRows) = rowSeparatorGeneric "\n" a (0, previousRows)
    rowSeparator a (index, previousRows) = rowSeparatorGeneric ",\n" a (index, previousRows)
    rowSeparatorGeneric sep a (index, previousRows) = (index+1, previousRows <> sep <> "(" <> elementToQueryFormat a <> ")")
    (_, assembledRows) = foldr rowSeparator (0, "") collection

assembledRowsFormatter :: (QueryFormat queryFormat) => InsertType -> queryFormat -> queryFormat -> queryFormat
assembledRowsFormatter t assembledRows assembledColList = case t of
  WrapInSelect -> "SELECT * FROM (" <> coreContent <> ") as values_selection " <> assembledColList
  PureValues -> coreContent
  where coreContent = "VALUES " <> assembledRows
