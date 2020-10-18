{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Massalia.SQLSelectStruct
  ( SelectStruct (..),
    QueryAndDecoder (..),
    compositeToListArray,
    queryAndDecoderToQueryFormat,
    selectStructToListSubquery,
    decoderToListSubdecoder,
    compositeToListDecoderTuple,
    selectStructToRecordSubquery,
    compositeToDecoderTuple,
    selectStructToQueryFormat,
    queryAndDecoderToListSubquery,
    queryAndDecoderToSnippetAndResult,
    queryAndDecoderToStatement,
    queryAndDecoderToSession,
    concatAnd,
    inlineAndUnion,
    filterMerge,
    simpleWhereEq,
  )
where

import qualified Massalia.HasqlDec as Decoders
import Massalia.HasqlExec
  ( Session,
    Statement,
    dynamicallyParameterized,
    dynamicallyParameterizedStatement,
  )
import Massalia.QueryFormat
  ( BinaryQuery,
    QueryFormat,
    DecodeTuple(DecodeTuple),
    simpleEq,
    fromText,
    decodeNameInContext,
    MassaliaContext,
  )
import Massalia.Utils (intercalate, inParens)
import Protolude hiding (intercalate)

data QueryAndDecoder queryFormat decoder
  = QueryAndDecoder
      { query :: SelectStruct queryFormat,
        decoder :: Decoders.Composite decoder
      }

-- | select query to a HASQL Statement, which can be executed in a Session.
queryAndDecoderToStatement :: QueryAndDecoder BinaryQuery decoder -> Statement () [decoder]
queryAndDecoderToStatement selectSt = dynamicallyParameterized snippet result
  where
    (snippet, result) = queryAndDecoderToSnippetAndResult selectSt

-- | Transforms a given 'QueryAndDecoder' to a its assembled query format.
queryAndDecoderToQueryFormat :: (QueryFormat qf) => QueryAndDecoder qf decoder -> qf
queryAndDecoderToQueryFormat = selectStructToQueryFormat . query

-- | select query to a HASQL Statement, which can be executed in a Session.
queryAndDecoderToSession :: QueryAndDecoder BinaryQuery decoder -> Session [decoder]
queryAndDecoderToSession selectSt = dynamicallyParameterizedStatement snippet result
  where
    (snippet, result) = queryAndDecoderToSnippetAndResult selectSt

-- | select query to a HASQL Statement, which can be executed in a Session.
queryAndDecoderToSnippetAndResult :: QueryAndDecoder BinaryQuery decoder -> (BinaryQuery, Decoders.Result [decoder])
queryAndDecoderToSnippetAndResult selectSt = (content, decoderValue)
  where
    content = assembleSelectStruct [Row] (query selectSt)
    decoderValue = Decoders.rowList (Decoders.column $ Decoders.nonNullable $ Decoders.composite $ decoder selectSt)

queryAndDecoderToListSubquery ::
  QueryFormat queryFormat =>
  QueryAndDecoder queryFormat decoder -> (queryFormat, DecodeTuple [decoder])
queryAndDecoderToListSubquery struct = (assembled, DecodeTuple newDecoder Decoders.nonNullable)
  where
    assembled = selectStructToListSubquery (query struct)
    newDecoder = compositeToListArray $ decoder struct
 
compositeToListArray :: Decoders.Composite element -> Decoders.Value [element]
compositeToListArray = valueToListArray . Decoders.composite
valueToListArray :: Decoders.Value element -> Decoders.Value [element]
valueToListArray = Decoders.listArray . Decoders.nonNullable
compositeToListDecoderTuple :: Decoders.Composite decoder -> DecodeTuple [decoder]
compositeToListDecoderTuple input = DecodeTuple (compositeToListArray input) Decoders.nonNullable
compositeToDecoderTuple :: Decoders.Composite decoder -> DecodeTuple decoder
compositeToDecoderTuple input = DecodeTuple (Decoders.composite input) Decoders.nonNullable
decoderToListSubdecoder :: DecodeTuple decoder -> DecodeTuple [decoder]
decoderToListSubdecoder (DecodeTuple dec _) = DecodeTuple (valueToListArray dec) Decoders.nonNullable


selectStructToListSubquery ::
  QueryFormat queryFormat =>
  SelectStruct queryFormat -> queryFormat
selectStructToListSubquery = inParens . (assembleSelectStruct [CoalesceArr, ArrayAgg, Row])
selectStructToRecordSubquery ::
  QueryFormat queryFormat =>
  SelectStruct queryFormat -> queryFormat
selectStructToRecordSubquery = inParens . (assembleSelectStruct [Row])

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
assembleSelectStruct wrapValueList struct = prefixCTERes <>
  selectRes <>
  fromRes <>
  joinRes <>
  whereRes <>
  groupByRes <> havingRes <>
  (if wasAggregated then "" else orderByRes <> offsetLimitRes)
  where
    sectionSep = " "
    pref = (sectionSep <>)
    prefixCTERes = fromMaybe mempty $ (<> sectionSep) <$> _rawPrefix struct
    selectRes = "SELECT " <> selectPartialRes
    (wasAggregated, selectPartialRes) = (wrapList wrapValueList (orderByRes, offsetLimitResAg) $ (False, intercalate ", " $ _select struct))
    fromRes = fromMaybe mempty $ (pref "FROM " <>) <$> _from struct
    joinRes = intercalIfExists sectionSep sectionSep $ _join struct
    whereRes = fromMaybe mempty $ (pref "WHERE " <>) <$> _where struct
    groupByRes = intercalIfExists (pref "GROUP BY ") ", " (_groupBy struct)
    havingRes = fromMaybe mempty $ (pref "HAVING " <>) <$> _having struct
    orderByRes = intercalIfExists (pref "ORDER BY ") ", " (_orderBy struct)
    offsetLimitResAg = offsetLimitToQF Array pref ofsetLimitVal
    offsetLimitRes = offsetLimitToQF Plain pref ofsetLimitVal
    ofsetLimitVal = _offsetLimit struct

wrap :: (Semigroup a, IsString a) => SQLWrapper -> (a, a) -> (Bool, a) -> (Bool, a)
wrap Row _ (wasAggregated, selectVal) = (wasAggregated, "row(" <> selectVal <> ")")
wrap ArrayAgg (orderByVal, offsetLimit) (_, selectVal) = (True, "(array_agg(" <> selectVal <> " " <> orderByVal <> "))" <> offsetLimit)
wrap CoalesceArr _ (wasAggregated, selectVal) = (wasAggregated, "coalesce(" <> selectVal <> ", '{}')")

-- | The wrapping may involve an aggregation, in which case we have to provide the order By,
--  at the select stage.
wrapList :: (Semigroup t, IsString t) => [SQLWrapper] -> (t, t) -> (Bool, t) -> (Bool, t)
wrapList [] _ q = q
wrapList (!x:xs) odb !q = wrap x odb (wrapList xs odb $ q)

intercalIfExists :: (QueryFormat qf) => qf -> qf -> [qf] -> qf
intercalIfExists _ _ [] = mempty
intercalIfExists prefix sep givenList = prefix <> intercalate sep givenList

data SQLWrapper = CoalesceArr | Row | ArrayAgg

-- | This is a very simple struct object meant to
-- simplify query building (it's not meant to bring safety).
data SelectStruct queryFormat = SelectStruct {
  _rawPrefix  :: !(Maybe queryFormat),
  _select  :: ![queryFormat],
  _from  :: !(Maybe queryFormat),
  _join  :: ![queryFormat],
  _where  :: !(Maybe queryFormat),
  _groupBy  :: ![queryFormat],
  _having  :: !(Maybe queryFormat),
  _orderBy  :: ![queryFormat],
  _offsetLimit  :: !(Maybe (Maybe queryFormat, queryFormat))
} deriving (Eq, Show)

instance (Semigroup queryFormat) => Semigroup (SelectStruct queryFormat) where
  (<>) a b = SelectStruct {
    _rawPrefix = _rawPrefix a <> _rawPrefix b,
    _select = _select a <> _select b,
    _from = _from a <> _from b,
    _join = _join a <> _join b,
    _where = _where a <> _where b,
    _groupBy = _groupBy a <> _groupBy b,
    _having = _having a <> _having b,
    _orderBy = _orderBy a <> _orderBy b,
    _offsetLimit = case (_offsetLimit a, _offsetLimit b) of
      (Nothing, Just bbo) -> Just bbo
      (Just aao, _) -> Just aao
      _ -> Nothing
  }

-- | An alternative to the Semigroup concat between two queries with AND 
-- in _where and _having
concatAnd :: (QueryFormat qf) => SelectStruct qf -> SelectStruct qf -> SelectStruct qf
concatAnd a b = SelectStruct {
    _rawPrefix = concatMaybeSQL " " (_rawPrefix a) (_rawPrefix b),
    _select = _select a <> _select b,
    _from = _from a <> _from b,
    _join = _join a <> _join b,
    _where = concatMaybeSQL " AND " (_where a) (_where b),
    _groupBy = _groupBy a <> _groupBy b,
    _having = concatMaybeSQL " AND "  (_having a) (_having b),
    _orderBy = _orderBy a <> _orderBy b,
    _offsetLimit = case (_offsetLimit a, _offsetLimit b) of
      (Nothing, Just bbo) -> Just bbo
      (Just aao, _) -> Just aao
      _ -> Nothing
  }

concatMaybeSQL :: Semigroup a => a -> Maybe a -> Maybe a -> Maybe a
concatMaybeSQL sep a1 b1 = case (a1, b1) of
  (Just a, Just b) -> Just (a <> sep <> b)
  (Just a, Nothing) -> Just a
  (Nothing, Just b) -> Just b
  (Nothing, Nothing) -> Nothing
xorOrLeft :: Semigroup a => Maybe a -> Maybe a -> Maybe a
xorOrLeft a1 b1 = case (a1, b1) of
  (Just a, _) -> Just a
  (Nothing, Just b) -> Just b
  (Nothing, Nothing) -> Nothing

instance (Semigroup queryFormat) => Monoid (SelectStruct queryFormat) where
  mempty = SelectStruct {
    _rawPrefix = Nothing,
    _select = mempty,
    _from = Nothing,
    _join = mempty,
    _where = Nothing,
    _groupBy = mempty,
    _having = Nothing,
    _orderBy = mempty,
    _offsetLimit = Nothing
  }

filterMerge :: (Semigroup queryFormat, IsString queryFormat) => SelectStruct queryFormat -> SelectStruct queryFormat -> SelectStruct queryFormat
filterMerge a b = a {
    _rawPrefix = _rawPrefix a <> _rawPrefix b,
    _select = _select a <> _select b,
    _from = xorOrLeft (_from a) (_from b),
    _join = _join a <> _join b,
    _where = concatMaybeSQL " AND " (_where a) (_where b),
    _groupBy = _groupBy a <> _groupBy b,
    _orderBy = _orderBy a <> _orderBy b,
    _having = concatMaybeSQL " AND " (_having a) (_having b),
    _offsetLimit = xorOrLeft (_offsetLimit a) (_offsetLimit b)
  }

inlineAndUnion :: (QueryFormat qf) => SelectStruct qf -> [SelectStruct qf] -> SelectStruct qf
inlineAndUnion input [] = input
inlineAndUnion input givenList = input{
    _from = Just result
  }
  where
    result = "((" <> foldMap identity unioned <> "))" <> (fromMaybe "" $ _from input)
    unioned = intersperse ") UNION (" inlined
    inlined = assembleSelectStruct [] <$> givenList

simpleWhereEq :: (QueryFormat queryFormat, MassaliaContext a) => queryFormat -> Text -> queryFormat -> a -> queryFormat -> SelectStruct queryFormat
simpleWhereEq lName tName rName context name = mempty
          { _where = Just (simpleEq name lName decodedName rName)
          }
  where decodedName = fromText (decodeNameInContext context tName)  

data PaginQueryFormat
  -- | The plain query format is the classic Offset Limit tuple at the end of a select query.
  = Plain 
  -- | The array query format is a limit offset logic for array_aggregated values 
  -- it's a simple array accessor tuple such as @[offsetValue, limitValue]@
  | Array
  deriving (Eq)

offsetLimitToQF ::
  (QueryFormat qf) =>
  PaginQueryFormat ->
  (qf -> qf) ->
  (Maybe (Maybe qf, qf)) ->
  qf
offsetLimitToQF formatType pref ol = case ol of
  Nothing -> mempty
  Just val -> case (val, formatType) of
    ((Nothing, limitVal), Plain) -> (pref "LIMIT ") <> limitVal
    ((Just offsetVal, limitVal), Plain) -> (pref "OFFSET ") <> offsetVal <> " LIMIT " <> limitVal
    ((offsetVal, limitVal), Array) -> "[" <> offsetValReal <> "+ 1" <> ":" <> (limitVal <> "+" <> offsetValReal) <> "]"
      where offsetValReal = fromMaybe "0" offsetVal