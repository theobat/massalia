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
    addOrderLimit,
    addWhereJoinGroup,
    structToSubquery,
    AQueryPart(AQueryPartConst),
    addSelectColumns,
    AssemblingOptions(..),
    defaultSelect
  )
where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import MassaliaUtils (intercalate, intercalateMap)
import Text.Inflections (toUnderscore)
import MassaliaQueryFormat (
    QueryFormat(param)
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

defaultSelect :: (QueryFormat content) => RawSelectStruct content  
defaultSelect = RawSelectStruct
      { wrapFunctionList = [Row], -- either: "row" or "array_agg", "row"
        selectPart = [],
        fromPart = "NOT_A_VALID_FROM",
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

rowFunctionToContent :: (IsString content) => RowFunction -> content
rowFunctionToContent Row = "row"
rowFunctionToContent ArrayAgg = "array_agg"

offsetLimitToContent :: (Monoid content, QueryFormat content) => Maybe (Int, Int) -> [content]
offsetLimitToContent Nothing = []
offsetLimitToContent (Just tuple) = pure $ case tuple of
  (0, _) -> limitSnippet
  (x, _) -> "OFFSET " <> (param $ toInt64 x) <> " " <> limitSnippet
  where
    limitSnippet = "LIMIT " <> (param $ toInt64 $ snd tuple)
    toInt64 x = fromIntegral x :: Int64

selectGroup :: (Monoid content, QueryFormat content) => [AQueryPart SQLSelect content] -> [RowFunction] -> content
selectGroup selectList functionList = "SELECT " <> rawGroup
  where
    rawGroup = foldr (\rowFunc acc -> rowFunctionToContent rowFunc <> "(" <> acc <> ")") joinedColumns functionList
    joinedColumns = intercalateMap getContent ", " selectList

-- | An assembly function to transform a structured query to a
-- content (which is either Text or Snippet)
structToContent ::
  (QueryFormat content, Monoid content) =>
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
  (QueryFormat content, Monoid content) =>
  AssemblingOptions content -> RawSelectStruct content -> AQueryPart partType content
structToSubquery a b = AQueryPartConst ( "(" <> structToContent a b <> ")" )

addWhereJoinGroup :: (Monoid content) =>
  AQueryPart SQLWhere content ->
  [AQueryPart SQLJoin content] ->
  [AQueryPart SQLGroupBy content] ->
  RawSelectStruct content ->
  RawSelectStruct content
addWhereJoinGroup whereAddition joinAddition groupByAddition currentQuery = currentQuery {
    whereConditions = case whereConditions currentQuery of
      Nothing -> Just whereAddition
      Just currentWhere -> Just $ currentWhere <> whereAddition,
    joinList = joinList currentQuery <> joinAddition,
    groupByList = groupByList currentQuery <> groupByAddition
  }


addOrderLimit :: (Monoid content) =>
  [AQueryPart SQLOrderBy content] ->
  OffsetLimit ->
  RawSelectStruct content ->
  RawSelectStruct content
addOrderLimit orderByAddition offsetLimitAddition currentQuery = currentQuery{
    offsetLimit = offsetLimitAddition,
    orderByList = orderByList currentQuery <> orderByAddition
  }

addSelectColumns :: [AQueryPart SQLSelect content] -> [RowFunction] -> RawSelectStruct content -> RawSelectStruct content
addSelectColumns columnList rowFunctionList currentQuery = currentQuery{
    selectPart = selectPart currentQuery <> columnList,
    wrapFunctionList = rowFunctionList <> wrapFunctionList currentQuery
  }