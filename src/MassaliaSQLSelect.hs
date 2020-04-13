{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaSQLSelect
  ( SelectStruct (..),
    RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    structToSubquery,
    AQueryPart(AQueryPartConst),
    getInitialValueSelect,
    AssemblingOptions(..),
    initSelect,
    selectStructToContent,
    selectStructToContentDefault,
    selectStructToSession,
    transformOrderLimit,
    transformWhereJoinGroup,
    addOrderLimit,
    addWhereJoinGroup,
    scalar,
    collection
  )
where

import MassaliaSQLRawSelect (
    RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    addOrderLimit,
    addWhereJoinGroup,
    structToSubquery,
    AQueryPart(AQueryPartConst),
    addSelectColumns,
    AssemblingOptions(..),
    initSelect
  )
import qualified Hasql.Decoders as Decoders
import Data.Text(Text)
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.DynamicStatements.Snippet as Snippet (param)
import qualified Hasql.Encoders as Encoders
import MassaliaQueryFormat (
    QueryFormat(param, fromText), HasqlSnippet
  )
import Text.Inflections (toUnderscore)
import Data.Maybe (fromMaybe)

data SelectStruct decoder queryFormat
  = SelectStruct
      { query :: RawSelectStruct queryFormat,
        decoder :: Decoders.Composite decoder
      }

getInitialValueSelect :: RawSelectStruct queryFormat -> recordType -> SelectStruct recordType queryFormat
getInitialValueSelect rawStruct defaultRecord = SelectStruct {
  query = rawStruct,
  decoder = pure defaultRecord
}
type Updater a decoder = decoder -> a -> decoder
type ValueDec a = Decoders.Value a

scalar :: (QueryFormat queryFormat) =>
  Text ->
  Text ->
  Updater a decoder ->
  ValueDec a ->
  SelectStruct decoder queryFormat ->
  SelectStruct decoder queryFormat
scalar tableName column = object $ tableName <> "." <> snakeColumn
  where
    snakeColumn = case toUnderscore column of
        Left _ -> error "Failed at transforming field name to underscore = " <> column
        Right e -> e

object :: (QueryFormat queryFormat) =>
  Text ->
  Updater a decoder ->
  ValueDec a ->
  SelectStruct decoder queryFormat ->
  SelectStruct decoder queryFormat
object expr updater hasqlValue selectStruct = selectStruct {
    query = addSelectColumns [fromText expr] [] (query selectStruct),
    decoder = do
      entity <- decoder selectStruct
      updater entity <$> Decoders.field (Decoders.nonNullable hasqlValue)
  }

type DecoderContainer wrapperType nestedRecordType = Decoders.NullableOrNot Decoders.Value nestedRecordType -> Decoders.Value (wrapperType nestedRecordType)
type UpdaterWrap nestedRecordType decoder = decoder -> nestedRecordType -> decoder

collection :: (QueryFormat queryFormat, Monoid (wrapperType nestedRecordType)) =>
    AssemblingOptions queryFormat ->
    DecoderContainer wrapperType nestedRecordType ->
    SelectStruct nestedRecordType queryFormat ->
    Updater (wrapperType nestedRecordType) recordType ->
    SelectStruct recordType queryFormat ->
    SelectStruct recordType queryFormat
collection assemblingOptions decoderInstance subQuery updater currentQuery = currentQuery {
    query = addSelectColumns [structToSubquery assemblingOptions subqueryContent] [] (query currentQuery),
    decoder = do
      entity <- decoder currentQuery
      (\e v -> updater e (fromMaybe mempty v)) entity <$> subQueryListDecoder
  }
  where
    subqueryContent = addSelectColumns [] [ArrayAgg] $ query subQuery
    subQueryListDecoder = Decoders.field (Decoders.nullable $ decoderInstance $ Decoders.nonNullable $ Decoders.composite (decoder subQuery))

selectStructToContent :: (QueryFormat content) => AssemblingOptions content -> SelectStruct decoder content -> content
selectStructToContent options = structToContent options . query

selectStructToContentDefault :: (QueryFormat content) => SelectStruct decoder content -> content
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

transformQuery :: (QueryFormat content) => (RawSelectStruct content -> RawSelectStruct content) -> SelectStruct decoder content -> SelectStruct decoder content
transformQuery transformer currentQuery = currentQuery{
  query = transformer $ query currentQuery
}
transformOrderLimit orderList limitTuple = transformQuery (addOrderLimit orderList limitTuple)
transformWhereJoinGroup wherePart joinPart groupPart = transformQuery (addWhereJoinGroup wherePart joinPart groupPart)