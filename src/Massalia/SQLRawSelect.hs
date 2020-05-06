{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Massalia.SQLRawSelect
-- Description : A simple Record & utility functions to represent an SQL SELECT statement. It's not meant to be
--  particularly safer than a literal SELECT query, but it allows for an orderless adjunction of elements easily
--  (say ORDER BY before SELECT for instance), whereas the literal SELECT does not.
module Massalia.SQLRawSelect
  ( RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    addOrderLimit,
    addWhereJoinGroup,
    structToSubquery,
    AQueryPart (AQueryPartConst),
    addSelectColumns,
    AssemblingOptions (..),
    initSelect,
    emptySelect,
  )
where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Massalia.QueryFormat
  (
    SQLEncoder (sqlEncode)
  )
import Massalia.SQLPart
  ( AQueryPart (AQueryPartConst),
    AssemblingOptions (..),
    defaultAssemblingOptions,
    getContent,
    getListContent,
    getMaybeContent,
    testAssemblingOptions,
  )
import Massalia.Utils (intercalate, intercalateMap, toUnderscore)

-- | A simple data structure to model an SQL select query.
-- The 'content' type parameter is meant to either be Text or Hasql's snippet type,
-- but it could be anything that's a 'QueryFormat' really.
data RawSelectStruct content
  = RawSelectStruct
      { wrapFunctionList :: [RowFunction], -- either: "row" or "array_agg", "row"
        selectPart :: [AQueryPart SQLSelect content],
        fromPart :: AQueryPart SQLFrom content,
        joinList :: [AQueryPart SQLJoin content],
        whereConditions :: Maybe (AQueryPart SQLWhere content),
        groupByList :: [AQueryPart SQLGroupBy content],
        havingConditions :: Maybe (AQueryPart SQLWhere content),
        orderByList :: [AQueryPart SQLOrderBy content],
        offsetLimit :: OffsetLimit
      }

type OffsetLimit = Maybe (Int, Int)

-- | The initial select struct for Massalia use cases.
initSelect :: (IsString content) => RawSelectStruct content
initSelect =
  RawSelectStruct
    { wrapFunctionList = [Row],
      selectPart = [],
      fromPart = "NOT_A_VALID_FROM",
      joinList = [],
      whereConditions = Nothing,
      groupByList = [],
      havingConditions = Nothing,
      orderByList = [],
      offsetLimit = Nothing
    }

-- | This is the neutral element for the monoid instance of the 'RawSelectStruct' struct.
emptySelect :: (IsString content) => RawSelectStruct content
emptySelect =
  RawSelectStruct
    { wrapFunctionList = [], -- either: "row" or "array_agg", "row"
      selectPart = [],
      fromPart = "",
      joinList = [],
      whereConditions = Nothing,
      groupByList = [],
      havingConditions = Nothing,
      orderByList = [],
      offsetLimit = Nothing
    }

data SQLSelect

data SQLFrom

data SQLJoin

data SQLWhere

data SQLGroupBy

data SQLOrderBy

data RowFunction = Row | ArrayAgg

rowFunctionToContent :: (IsString queryFormat) => RowFunction -> queryFormat
rowFunctionToContent Row = "row"
rowFunctionToContent ArrayAgg = "array_agg"

offsetLimitToContent :: (
    Monoid queryFormat, SQLEncoder Int64 queryFormat
  ) => Maybe (Int, Int) -> [queryFormat]
offsetLimitToContent Nothing = []
offsetLimitToContent (Just tuple) = pure $ case tuple of
  (0, _) -> limitSnippet
  (x, _) -> "OFFSET " <> (sqlEncode $ toInt64 x) <> " " <> limitSnippet
  where
    limitSnippet = "LIMIT " <> (sqlEncode $ toInt64 $ snd tuple)
    toInt64 x = fromIntegral x :: Int64

selectGroup :: (Monoid content, IsString content) => [AQueryPart SQLSelect content] -> [RowFunction] -> content
selectGroup selectList functionList = "SELECT " <> rawGroup
  where
    rawGroup = foldr (\rowFunc acc -> rowFunctionToContent rowFunc <> "(" <> acc <> ")") joinedColumns functionList
    joinedColumns = intercalateMap getContent ", " selectList

-- | An assembly function to transform a structured query to a
-- content (which is either Text or Snippet)
structToContent ::
  (Monoid content, SQLEncoder Int64 content) =>
  AssemblingOptions content ->
  RawSelectStruct content ->
  content
structToContent
  options
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
      ( basicQueryParts
          <> getListContent partSepValue "" joinListValue
          <> getMaybeContent "WHERE " whereConditionsValue
          <> getListContent innerSepValue "GROUP BY " groupByListValue
          <> getMaybeContent "HAVING " havingConditionsValue
          <> getListContent innerSepValue "ORDER BY " orderByListValue
          <> offsetLimitToContent offsetLimitValue
      )
    where
      basicQueryParts =
        [ selectGroup selectPartValue wrapFunctionListValue,
          "FROM " <> getContent fromPartValue
        ]
      partSepValue = partSeparator options
      innerSepValue = innerSeparator options

structToSubquery ::
  (Monoid content, SQLEncoder Int64 content) =>
  AssemblingOptions content ->
  RawSelectStruct content ->
  AQueryPart SQLSelect content
structToSubquery a b = AQueryPartConst ("(" <> structToContent a b <> ")")

addWhereJoinGroup ::
  (Monoid content) =>
  AQueryPart SQLWhere content ->
  [AQueryPart SQLJoin content] ->
  [AQueryPart SQLGroupBy content] ->
  RawSelectStruct content ->
  RawSelectStruct content
addWhereJoinGroup whereAddition joinAddition groupByAddition currentQuery =
  currentQuery
    { whereConditions = case whereConditions currentQuery of
        Nothing -> Just whereAddition
        Just currentWhere -> Just $ currentWhere <> whereAddition,
      joinList = joinList currentQuery <> joinAddition,
      groupByList = groupByList currentQuery <> groupByAddition
    }

addOrderLimit ::
  (Monoid content) =>
  [AQueryPart SQLOrderBy content] ->
  OffsetLimit ->
  RawSelectStruct content ->
  RawSelectStruct content
addOrderLimit orderByAddition offsetLimitAddition currentQuery =
  currentQuery
    { offsetLimit = offsetLimitAddition,
      orderByList = orderByList currentQuery <> orderByAddition
    }

addSelectColumns :: [AQueryPart SQLSelect content] -> [RowFunction] -> RawSelectStruct content -> RawSelectStruct content
addSelectColumns columnList rowFunctionList currentQuery =
  currentQuery
    { selectPart = selectPart currentQuery <> columnList,
      wrapFunctionList = rowFunctionList <> wrapFunctionList currentQuery
    }
