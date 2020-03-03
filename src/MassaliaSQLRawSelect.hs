{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaSQLRawSelect
  ( RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    furtherQualifyWhereJoin,
    structToSubquery,
    ASelectQueryPart(SelectQueryPart),
    addSelectColumns,
    AssemblingOptions(..)
  )
where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import MassaliaCore (MassaliaStruct (..))
import MassaliaUtils (intercalate, intercalateMap)
import Text.Inflections (toUnderscore)
import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))
import MassaliaEncoder (DynamicParameters(param))
import MassaliaSQLPart (
    ASelectQueryPart(SelectQueryPart),
    getContent,
    getListContent,
    getMaybeContent,
    AssemblingOptions(..),
    defaultAssemblingOptions,
    testAssemblingOptions
  )

-- | A simple data structure to model an SQL select query.
-- | The 'content' type parameter is meant to either be Text or 
data RawSelectStruct content
  = RawSelectStruct
      { wrapFunctionList :: [RowFunction], -- either: "row" or "array_agg", "row"
        selectPart :: [ASelectQueryPart SQLSelect content],
        fromPart :: ASelectQueryPart SQLFrom content,
        joinList :: [ASelectQueryPart SQLJoin content],
        whereConditions :: Maybe (ASelectQueryPart SQLWhere content),
        groupByList :: [ASelectQueryPart SQLGroupBy content],
        havingConditions :: Maybe (ASelectQueryPart SQLWhere content),
        orderByList :: [ASelectQueryPart SQLOrderBy content],
        offsetLimit :: Maybe (Int, Int)
      }

data SQLSelect

data SQLFrom

data SQLJoin

data SQLWhere

data SQLGroupBy

data SQLOrderBy

data RowFunction = Row | ArrayAgg

rowFunctionToContent :: (IsString content) => RowFunction -> content
rowFunctionToContent Row = "row"
rowFunctionToContent ArrayAgg = "array_agg"

offsetLimitToContent :: (Monoid content, IsString content, DynamicParameters content) => Maybe (Int, Int) -> [content]
offsetLimitToContent Nothing = []
offsetLimitToContent (Just tuple) = pure $ case tuple of
  (0, _) -> limitSnippet
  (x, _) -> "OFFSET " <> (param $ toInt64 x) <> " " <> limitSnippet
  where
    limitSnippet = "LIMIT " <> (param $ toInt64 $ snd tuple)
    toInt64 x = fromIntegral x :: Int64

-- selectFields = foldr (\(SelectQueryPart fieldSnippet) acc -> joinSnippet fieldSnippet acc) mempty
--   where
--     joinSnippet fieldSnippet "" = fieldSnippet
--     joinSnippet fieldSnippet acc = acc <> "," <> fieldSnippet

selectGroup :: (Monoid content, IsString content) => [ASelectQueryPart SQLSelect content] -> [RowFunction] -> content
selectGroup selectList functionList = "SELECT " <> rawGroup
  where
    rawGroup = foldr (\rowFunc acc -> rowFunctionToContent rowFunc <> "(" <> acc <> ")") joinedColumns functionList
    joinedColumns = intercalateMap getContent ", " selectList

-- | An “assembly” function to transform a structured query to a
-- | content (which is either Text or Snippet)
-- |
structToContent ::
  (DynamicParameters content, IsString content, Monoid content) =>
  AssemblingOptions content -> RawSelectStruct content -> content
structToContent options
  RawSelectStruct
    { wrapFunctionList = wrapFunctionListValue,
      selectPart = selectPartValue,
      fromPart = fromPartValue,
      joinList = joinListValue,
      whereConditions = whereConditionsValue,
      groupByList = groupByListValue,
      havingConditions = havingConditionsValue,
      orderByList = orderByListValue,
      offsetLimit = offsetLimitValue
    } =
    intercalate
      partSepValue
      (
          basicQueryParts <>
          getListContent partSepValue "" joinListValue <>
          getMaybeContent "WHERE " whereConditionsValue <>
          getListContent innerSepValue "GROUP BY " groupByListValue <>
          getMaybeContent "HAVING " havingConditionsValue <>
          getListContent innerSepValue "ORDER BY " orderByListValue <>
          offsetLimitToContent offsetLimitValue
      )
    where
      basicQueryParts = [
          selectGroup selectPartValue wrapFunctionListValue,
          "FROM " <> getContent fromPartValue
        ]
      partSepValue = partSeparator options
      innerSepValue = innerSeparator options

structToSubquery ::
  (DynamicParameters content, IsString content, Monoid content) =>
  AssemblingOptions content -> RawSelectStruct content -> ASelectQueryPart partType content
structToSubquery a b = SelectQueryPart ( "(" <> structToContent a b <> ")" )

furtherQualifyWhereJoin :: (Monoid content) =>
  ASelectQueryPart SQLWhere content ->
  [ASelectQueryPart SQLJoin content] ->
  RawSelectStruct content ->
  RawSelectStruct content
furtherQualifyWhereJoin whereAddition joinAddition currentQuery = currentQuery {
    whereConditions = (\currentWhere -> currentWhere <> whereAddition) <$> whereConditions currentQuery,
    joinList = joinList currentQuery <> joinAddition
  }

addSelectColumns :: [ASelectQueryPart SQLSelect content] -> RawSelectStruct content -> RawSelectStruct content
addSelectColumns columnList currentQuery = currentQuery {
    selectPart = selectPart currentQuery <> columnList
  }