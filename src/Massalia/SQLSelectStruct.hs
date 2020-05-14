{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Massalia.SQLSelectStruct
  ( SelectStruct (..),
    QueryAndDecoder (..),
    compositeToListArray,
    selectStructToListSubquery,
    selectStructToQueryFormat,
    queryAndDecoderToListSubquery,
    queryAndDecoderToSnippetAndResult,
    queryAndDecoderToStatement,
    queryAndDecoderToSession
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
    FromText(fromText),
    QueryFormat,
    commaSepInParens
  )
import Massalia.Utils (toUnderscore, intercalate, inParens)
import Protolude hiding (intercalate)

data QueryAndDecoder queryFormat decoder
  = QueryAndDecoder
      { query :: SelectStruct queryFormat,
        decoder :: Decoders.Composite decoder
      }

-- | select query to a HASQL Statement, which can be executed in a Session.
queryAndDecoderToStatement :: QueryAndDecoder HasqlSnippet decoder -> Statement () [decoder]
queryAndDecoderToStatement selectSt = dynamicallyParameterized snippet result
  where
    (snippet, result) = queryAndDecoderToSnippetAndResult selectSt

-- | select query to a HASQL Statement, which can be executed in a Session.
queryAndDecoderToSession :: QueryAndDecoder HasqlSnippet decoder -> Session [decoder]
queryAndDecoderToSession selectSt = dynamicallyParameterizedStatement snippet result
  where
    (snippet, result) = queryAndDecoderToSnippetAndResult selectSt

-- | select query to a HASQL Statement, which can be executed in a Session.
queryAndDecoderToSnippetAndResult :: QueryAndDecoder HasqlSnippet decoder -> (HasqlSnippet, Decoders.Result [decoder])
queryAndDecoderToSnippetAndResult selectSt = (content, decoderValue)
  where
    content = assembleSelectStruct [Row] (query selectSt)
    decoderValue = Decoders.rowList (Decoders.column $ Decoders.nonNullable $ Decoders.composite $ decoder selectSt)

queryAndDecoderToListSubquery ::
  QueryFormat queryFormat =>
  QueryAndDecoder queryFormat decoder -> (queryFormat, Decoders.Value [decoder])
queryAndDecoderToListSubquery struct = (assembled, newDecoder)
  where
    assembled = selectStructToListSubquery (query struct)
    newDecoder = compositeToListArray $ decoder struct
 
compositeToListArray = Decoders.listArray . Decoders.nonNullable . Decoders.composite

selectStructToListSubquery ::
  QueryFormat queryFormat =>
  SelectStruct queryFormat -> queryFormat
selectStructToListSubquery = inParens . (assembleSelectStruct [CoalesceArr, ArrayAgg, Row])

-- selectStructToSubquery ::
--   Maybe (Decoders.NullableOrNot Decoders.Value element -> Value (collection element)) ->
--   SelectStruct decoder queryFormat -> (queryFormat, Decoders.Value collection decoder)

selectStructToQueryFormat :: (QueryFormat queryFormat) =>
  SelectStruct queryFormat ->
  queryFormat
selectStructToQueryFormat = assembleSelectStruct []

-- | A simple assembler function to turn the SelectStruct struct into a proper query string.
--
-- Example:
-- >>> assembleSelectStruct $ mempty{_select = [1] }
-- SELECT 1
assembleSelectStruct :: (QueryFormat queryFormat) =>
  [SQLWrapper] ->
  SelectStruct queryFormat ->
  queryFormat
assembleSelectStruct wrapValueList struct = selectRes <>
  fromRes <>
  joinRes <>
  whereRes <>
  groupByRes <> havingRes <>
  orderByRes <>
  offsetLimitRes
  where
    sectionSep = " "
    pref = (sectionSep <>)
    selectRes = "SELECT " <> (wrapList wrapValueList $ intercalate ", " $ _select struct)
    fromRes = fromMaybe mempty $ (pref "FROM " <>) <$> _from struct
    joinRes = intercalIfExists sectionSep sectionSep $ _join struct
    whereRes = fromMaybe mempty $ (pref "WHERE " <>) <$> _where struct
    groupByRes = intercalIfExists (pref "GROUP BY ") ", " (_groupBy struct)
    havingRes = fromMaybe mempty $ (pref "HAVING " <>) <$> _having struct
    orderByRes = intercalIfExists (pref "ORDER BY ") ", " (_orderBy struct)
    offsetLimitRes = case _offsetLimit struct of
      Nothing -> mempty
      Just (Nothing, limitVal) -> (pref "LIMIT ") <> limitVal
      Just (Just offsetVal, limitVal) -> (pref "OFFSET ") <> offsetVal <> " LIMIT " <> limitVal 

wrap Row q = "row(" <> q <> ")"
wrap ArrayAgg q = "array_agg(" <> q <> ")"
wrap CoalesceArr q = "coalesce(" <> q <> ", '{}')"

wrapList [] q = q 
wrapList (x:xs) q =  wrap x (wrapList xs $ q)

intercalIfExists :: (QueryFormat queryFormat) => queryFormat -> queryFormat -> [queryFormat] -> queryFormat
intercalIfExists _ _ [] = mempty
intercalIfExists prefix sep list = prefix <> intercalate sep list

data SQLWrapper = CoalesceArr | Row | ArrayAgg

-- | This is a very simple struct object meant to
-- simplify query building (it's not meant to bring safety).
data SelectStruct queryFormat = SelectStruct {
  _select  :: [queryFormat],
  _from  :: Maybe queryFormat,
  _join  :: [queryFormat],
  _where  :: Maybe queryFormat,
  _groupBy  :: [queryFormat],
  _having  :: Maybe queryFormat,
  _orderBy  :: [queryFormat],
  _offsetLimit  :: Maybe (Maybe queryFormat, queryFormat)
} deriving (Eq, Show)

instance (Semigroup queryFormat) => Semigroup (SelectStruct queryFormat) where
  (<>) a b = SelectStruct {
    _select = _select a <> _select b,
    _from = _from a <> _from b,
    _join = _join a <> _join b,
    _where = _where a <> _where b,
    _groupBy = _groupBy a <> _groupBy b,
    _having = _having a <> _having b,
    _orderBy = _orderBy a <> _orderBy b,
    _offsetLimit = _offsetLimit a <> _offsetLimit b
  }    

instance (Semigroup queryFormat) => Monoid (SelectStruct queryFormat) where
  mempty = SelectStruct {
    _select = mempty,
    _from = Nothing,
    _join = mempty,
    _where = Nothing,
    _groupBy = mempty,
    _having = Nothing,
    _orderBy = mempty,
    _offsetLimit = Nothing
  }
