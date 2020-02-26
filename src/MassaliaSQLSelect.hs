{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module MassaliaSQLSelect
  ( SelectStruct (..),
    RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    furtherQualifyWhereJoin,
    structToSubquery
  )
where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet (param)
import qualified Hasql.Encoders as Encoders
import MassaliaCore (MassaliaStruct (..))
import MassaliaUtils (intercalate, intercalateMap)
import Text.Inflections (toUnderscore)
import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))

data SelectStruct decoder content
  = SelectStruct
      { query :: RawSelectStruct content,
        decoder :: Decoders.Composite decoder
      }

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

class DynamicParameters content where
  param :: (Show a, DefaultParamEncoder a) => a -> content

instance DynamicParameters Snippet where
  param = Snippet.param
instance DynamicParameters Text where
  param = pack . show
instance DynamicParameters String where
  param = show
  
newtype ASelectQueryPart partType content = SelectQueryPart content
type SelectQueryPartSnippet partType = ASelectQueryPart partType Snippet
type SelectQueryPartText partType = ASelectQueryPart partType Text
deriving instance (IsString content) => IsString (ASelectQueryPart partType content)
deriving instance (Semigroup content) => Semigroup (ASelectQueryPart partType content)
deriving instance (Monoid content) => Monoid (ASelectQueryPart partType content)

getContent :: ASelectQueryPart partType content -> content
getContent (SelectQueryPart content) = content

getListContent :: (Monoid content) => content -> ASelectQueryPart partType content -> [ASelectQueryPart partType content] -> [content]
getListContent separator _ [] = []
getListContent separator prefix valueList = [(getContent prefix) <> intercalate separator (getContent <$> valueList)]

getMaybeContent :: (Monoid content) => ASelectQueryPart partType content -> Maybe (ASelectQueryPart partType content) -> [content]
getMaybeContent _ Nothing = []
getMaybeContent prefix (Just value) = [(getContent prefix) <> getContent value]

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

data AssemblingOptions content = AssemblingOptions {
  partSeparator :: content,
  innerSeparator :: content
}
defaultAssemblingOptions = AssemblingOptions {
  partSeparator = "\n",
  innerSeparator = ","
}
testAssemblingOptions = AssemblingOptions {
  partSeparator = " ",
  innerSeparator = ", "
}
