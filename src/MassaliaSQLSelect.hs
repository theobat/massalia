{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaSQLSelect
  ( SelectStruct (..),
    RawSelectStruct (..),
    structToSnippet,
    RowFunction (..)
  )
where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet, param)
import qualified Hasql.Encoders as Encoders
import MassaliaCore (MassaliaStruct (..))
import Text.Inflections (toUnderscore)
import MassaliaUtils (intercalateMap, intercalate)

data SelectStruct decoder
  = SelectStruct
      { query :: RawSelectStruct,
        decoder :: Decoders.Composite decoder
      }

data RawSelectStruct
  = RawSelectStruct
      { wrapFunctionList :: [RowFunction], -- either: "row" or "array_agg", "row"
        selectPart :: [SelectQueryPart SQLSelect],
        fromPart :: SelectQueryPart SQLFrom,
        joinList :: [SelectQueryPart SQLJoin],
        whereConditions :: SelectQueryPart SQLWhere,
        groupByList :: [SelectQueryPart SQLGroupBy],
        havingConditions :: SelectQueryPart SQLWhere,
        orderByList :: [SelectQueryPart SQLOrderBy],
        offsetLimit :: Maybe (Int, Int)
      }

data SQLSelect

data SQLFrom

data SQLJoin

data SQLWhere

data SQLGroupBy

data SQLOrderBy

newtype SelectQueryPart a = SelectQueryPart Snippet deriving (IsString, Semigroup)

getSnippet :: SelectQueryPart a -> Snippet
getSnippet (SelectQueryPart val) = val

data RowFunction = Row | ArrayAgg | RowFunction Snippet

rowFunctionToSnippet :: RowFunction -> Snippet
rowFunctionToSnippet Row = "row"
rowFunctionToSnippet ArrayAgg = "array_agg"
rowFunctionToSnippet (RowFunction custom) = custom

offsetLimitToSnippet :: Maybe (Int, Int) -> Snippet
offsetLimitToSnippet Nothing = mempty
offsetLimitToSnippet (Just tuple) = case tuple of
  (0, _) -> limitSnippet
  (x, _) -> "OFFSET " <> (param $ toInt64 x) <> " " <> limitSnippet
  where
    limitSnippet = "LIMIT " <> (param $ toInt64 $ snd tuple)
    toInt64 x = (fromIntegral x) :: Int64

-- selectFields = foldr (\(SelectQueryPart fieldSnippet) acc -> joinSnippet fieldSnippet acc) mempty
--   where
--     joinSnippet fieldSnippet "" = fieldSnippet
--     joinSnippet fieldSnippet acc = acc <> "," <> fieldSnippet

selectGroup :: [SelectQueryPart SQLSelect] -> [RowFunction] -> Snippet
selectGroup selectList functionList = "SELECT " <> rawGroup
  where
    rawGroup = foldr (\rowFunc acc -> rowFunctionToSnippet rowFunc <> "(" <> acc <> ")") joinedColumns functionList
    joinedColumns = intercalateMap getSnippet ", " selectList

-- | An “assembly” function to transform a structured query to an (executable) 'Snippet'
-- |
structToSnippet :: RawSelectStruct -> Snippet
structToSnippet
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
      partSeparator
      [ selectGroup selectPartValue wrapFunctionListValue,
        "FROM " <> getSnippet fromPartValue,
        intercalate partSeparator (getSnippet <$> joinListValue),
        "WHERE " <> getSnippet whereConditionsValue,
        "GROUP BY " <> intercalate innerSeparator (getSnippet <$> groupByListValue),
        "HAVING " <> getSnippet havingConditionsValue,
        "ORDER BY " <> intercalate innerSeparator (getSnippet <$> orderByListValue),
        offsetLimitToSnippet offsetLimitValue
      ]
    where
      partSeparator = "\n"
      innerSeparator = ","
