{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Massalia.SQLInsert
-- Description : A module to ddefine an assembler function for creating insert into statements
module Massalia.SQLInsert
  (
    ValuesFormatType (..)
  )
where

import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Massalia.SQLClass (SQLValues(toSQLValues), SQLColumns(sqlColumns), SQLName(sqlName))
import Data.Text (Text)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Encoders as Encoders
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( HasqlSnippet,
    commaAssemble,
    inParens
  )
import Massalia.Utils (toUnderscore, intercalate)
import Protolude hiding (intercalate)

-- | The type of values formatting.
data ValuesFormatType
  = -- | select * from (values (1), (2)) as t;
    WrapInSelect
  | -- | insert into (values (1), (2)) as t;
    PureValues

data QueryShape = InsertShape ValuesFormatType | SelectShape

-- | A simple triplet @(tableName, columnList, haskellType -> queryFormat)@
-- newtype InsertSchema queryFormat underlyingType = InsertSchema (queryFormat, [queryFormat], underlyingType -> queryFormat)

-- valuesToInsertWrapped,
--   valuesToInsert,
--   valuesToSelect ::
--     (IsString queryFormat, Monoid queryFormat, Foldable collection) =>
--     -- | First is the tableName, then a column list and then the values encoder.
--     InsertSchema queryFormat underlyingType ->
--     -- | A collection of literal values to encode into the query.
--     collection underlyingType ->
--     -- | The end result for the insert query.
--     queryFormat
-- valuesToInsertWrapped = valuesToQueryFormat (InsertShape WrapInSelect)
-- valuesToInsert = valuesToQueryFormat (InsertShape PureValues)
-- valuesToSelect = valuesToQueryFormat SelectShape

-- | A function to create insert statements out of haskell records.
-- valuesToQueryFormat ::
--   (Monoid queryFormat, IsString queryFormat, Foldable collection) =>
--   -- | A parameter to specify how to wrap the literal values.
--   QueryShape ->
--   -- | First is the tableName, then a column list and then the values encoder.
--   InsertSchema queryFormat underlyingType ->
--   -- | A collection of literal values to encode into the query.
--   collection underlyingType ->
--   -- | The end result for the insert query.
--   queryFormat
-- valuesToQueryFormat queryShape (InsertSchema (tableName, header, elementToQueryFormat)) collection = result
--   where
--     result = case queryShape of
--       SelectShape -> partialRes WrapInSelect
--       InsertShape format -> partialRes format
--     partialRes formatType = valuesFormatter formatType columnListAssembled elementToQueryFormat collection
--     headerRes = "INSERT INTO \"" <> tableName <> "\" " <> columnListAssembled
--     columnListAssembled = "(" <> commaAssemble header <> ")"

-- columnsWrapper :: (Monoid queryFormat, IsString queryFormat, SQLColumns a) => queryFormat   
-- columnsWrapper = sqlColumns



selectValuesQuery ::
  forall collection recordType queryFormat. (
    Foldable collection,
    Functor collection,
    IsString queryFormat,
    Monoid queryFormat,
    SQLValues queryFormat recordType,
    SQLColumns recordType,
    SQLName recordType
  ) =>
  collection recordType -> queryFormat
selectValuesQuery recordCollection = result
  where
    result = assembledRowsFormatter WrapInSelect name assembledRows cols
    name = sqlName @recordType
    assembledRows = rowsAssembler " " listOfRows
    listOfRows = (inParens . intercalate ",") <$> listOfListOfValues
    listOfListOfValues = toSQLValues @queryFormat <$> recordCollection
    cols = intercalate "," $ sqlColumns @recordType


-- | A function to form a simple select query out of a collection of input values.
-- valuesFormatter ::
--   (Monoid queryFormat, IsString queryFormat, Foldable collection) =>
--   -- | A parameter to specify how to wrap the literal values. Default to 'WrapInSelect'.
--   ValuesFormatType ->
--   -- | A parameter to specify how to wrap the literal values.
--   queryFormat ->
--   -- | First is the tableName, then a column list and then the values encoder.
--   (underlyingType -> queryFormat) ->
--   -- | A collection of literal values to encode into the query.
--   collection underlyingType ->
--   -- | The end result for the insert query.
--   queryFormat
-- valuesFormatter formatType columnListAssembled elementToQueryFormat collection = partialRes
--   where
--     partialRes = assembledRowsFormatter formatType assembledRows columnListAssembled
--     rowSeparator a (0, previousRows) = rowSeparatorGeneric "\n" a (0, previousRows)
--     rowSeparator a (index, previousRows) = rowSeparatorGeneric ",\n" a (index, previousRows)
--     rowSeparatorGeneric sep a (index, previousRows) = (index + 1, previousRows <> sep <> "(" <> elementToQueryFormat a <> ")")
--     (_, assembledRows) = foldr rowSeparator (0, "") collection

rowsAssembler ::
  (Foldable collection, Semigroup queryFormat, IsString queryFormat) =>
  queryFormat -> collection queryFormat -> queryFormat
rowsAssembler sep input = assembledRows
  where
    rowSeparator a (0, previousRows) = rowSeparatorGeneric sep a (0, previousRows)
    rowSeparator a (index, previousRows) = rowSeparatorGeneric ("," <> sep) a (index, previousRows)
    rowSeparatorGeneric sep a (index, previousRows) = (index + 1, previousRows <> sep <> inParens a)
    (_, assembledRows) = foldr rowSeparator (0, "") input

assembledRowsFormatter :: (Monoid queryFormat, IsString queryFormat) => ValuesFormatType -> queryFormat -> queryFormat -> queryFormat -> queryFormat
assembledRowsFormatter t name assembledRows assembledColList = case t of
  WrapInSelect -> selectWrapper name assembledColList coreContent
  PureValues -> coreContent
  where
    coreContent = "VALUES " <> assembledRows

insertIntoWrapper ::
  (Monoid queryFormat, IsString queryFormat) => queryFormat -> queryFormat -> queryFormat
insertIntoWrapper tableName assembledColList = "INSERT INTO \"" <> tableName <> "\" " <> assembledColList

selectWrapper ::
  (Monoid queryFormat, IsString queryFormat) => queryFormat -> queryFormat -> queryFormat -> queryFormat
selectWrapper name assembledColList valueRows = "SELECT * FROM (" <> valueRows <> ") as " <> name <> " " <> assembledColList
