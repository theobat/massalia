{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Massalia.SQLSelect
  ( SelectStruct (..),
    RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    structToSubquery,
    AQueryPart (AQueryPartConst),
    getInitialValueSelect,
    AssemblingOptions (..),
    initSelect,
    selectStructToContent,
    selectStructToContentDefault,
    selectStructToSession,
    selectStructToSnippetAndResult,
    transformOrderLimit,
    transformWhereJoinGroup,
    addOrderLimit,
    addWhereJoinGroup,
    scalar,
    collection,
  )
where

import qualified Massalia.HasqlDec as Decoders
import qualified Massalia.HasqlEnc as Encoders
import Massalia.HasqlExec
  ( Session,
    Statement,
    dynamicallyParameterized,
    dynamicallyParameterizedStatement,
  )
import Massalia.QueryFormat
  ( HasqlSnippet,
    SQLEncoder (sqlEncode),
    FromText(fromText)
  )
import Massalia.SQLRawSelect
  ( AQueryPart (AQueryPartConst),
    AssemblingOptions (..),
    RawSelectStruct (..),
    RowFunction (..),
    addOrderLimit,
    addSelectColumns,
    addWhereJoinGroup,
    defaultAssemblingOptions,
    initSelect,
    structToContent,
    structToSubquery,
    testAssemblingOptions,
  )
import Massalia.Utils (toUnderscore)
import Protolude

data SelectStruct decoder queryFormat
  = SelectStruct
      { query :: RawSelectStruct queryFormat,
        decoder :: Decoders.Composite decoder
      }

getInitialValueSelect :: RawSelectStruct queryFormat -> recordType -> SelectStruct recordType queryFormat
getInitialValueSelect rawStruct defaultRecord =
  SelectStruct
    { query = rawStruct,
      decoder = pure defaultRecord
    }

type Updater a decoder = decoder -> a -> decoder

type ValueDec a = Decoders.Value a

scalar ::
  (FromText queryFormat) =>
  Text ->
  Text ->
  Updater a decoder ->
  ValueDec a ->
  SelectStruct decoder queryFormat ->
  SelectStruct decoder queryFormat
scalar tableName column = object $ tableName <> "." <> snakeColumn
  where
    snakeColumn = case toUnderscore column of
      Left _ -> panic "Failed at transforming field name to underscore = " <> column
      Right e -> e

object ::
  (FromText queryFormat) =>
  Text ->
  Updater a decoder ->
  ValueDec a ->
  SelectStruct decoder queryFormat ->
  SelectStruct decoder queryFormat
object expr updater hasqlValue selectStruct =
  selectStruct
    { query = addSelectColumns [fromText expr] [] (query selectStruct),
      decoder = do
        entity <- decoder selectStruct
        updater entity <$> Decoders.field (Decoders.nonNullable hasqlValue)
    }

type DecoderContainer wrapperType nestedRecordType = Decoders.NullableOrNot Decoders.Value nestedRecordType -> Decoders.Value (wrapperType nestedRecordType)

type UpdaterWrap nestedRecordType decoder = decoder -> nestedRecordType -> decoder

collection ::
  (SQLEncoder Int64 queryFormat, Monoid (wrapperType nestedRecordType)) =>
  AssemblingOptions queryFormat ->
  DecoderContainer wrapperType nestedRecordType ->
  SelectStruct nestedRecordType queryFormat ->
  Updater (wrapperType nestedRecordType) recordType ->
  SelectStruct recordType queryFormat ->
  SelectStruct recordType queryFormat
collection assemblingOptions decoderInstance subQuery updater currentQuery =
  currentQuery
    { query = addSelectColumns [structToSubquery assemblingOptions subqueryContent] [] (query currentQuery),
      decoder = do
        entity <- decoder currentQuery
        (\e v -> updater e (fromMaybe mempty v)) entity <$> subQueryListDecoder
    }
  where
    subqueryContent = addSelectColumns [] [ArrayAgg] $ query subQuery
    subQueryListDecoder = Decoders.field (Decoders.nullable $ decoderInstance $ Decoders.nonNullable $ Decoders.composite (decoder subQuery))

selectStructToContent :: (SQLEncoder Int64 content) => AssemblingOptions content -> SelectStruct decoder content -> content
selectStructToContent options = structToContent options . query

selectStructToContentDefault :: (SQLEncoder Int64 content) => SelectStruct decoder content -> content
selectStructToContentDefault = structToContent defaultAssemblingOptions . query

-- | select query to a HASQL Statement, which can be executed in a Session.
selectSnippetToStatement :: SelectStruct decoder HasqlSnippet -> Statement () [decoder]
selectSnippetToStatement selectSt = dynamicallyParameterized snippet result
  where
    (snippet, result) = selectStructToSnippetAndResult selectSt

-- | select query to a HASQL Statement, which can be executed in a Session.
selectStructToSession :: SelectStruct decoder HasqlSnippet -> Session [decoder]
selectStructToSession selectSt = dynamicallyParameterizedStatement snippet result
  where
    (snippet, result) = selectStructToSnippetAndResult selectSt

-- | select query to a HASQL Statement, which can be executed in a Session.
selectStructToSnippetAndResult :: SelectStruct decoder HasqlSnippet -> (HasqlSnippet, Decoders.Result [decoder])
selectStructToSnippetAndResult selectSt = (content, decoderValue)
  where
    content = selectStructToContentDefault selectSt
    decoderValue = Decoders.rowList (Decoders.column $ Decoders.nonNullable $ Decoders.composite $ decoder selectSt)

transformQuery :: (RawSelectStruct content -> RawSelectStruct content) -> SelectStruct decoder content -> SelectStruct decoder content
transformQuery transformer currentQuery =
  currentQuery
    { query = transformer $ query currentQuery
    }

transformOrderLimit orderList limitTuple = transformQuery (addOrderLimit orderList limitTuple)

transformWhereJoinGroup wherePart joinPart groupPart = transformQuery (addWhereJoinGroup wherePart joinPart groupPart)
