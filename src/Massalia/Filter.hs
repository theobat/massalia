{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.Filter
-- Description : A module to define an interface for simple scalar values filters.
module Massalia.Filter
  ( GQLFilterUUID,
    GQLFilterText,
    GQLScalarFilter (..),
    defaultScalarFilter,
    filterFieldToMabeContent,
    filterFieldToMaybeQueryPart,
  )
where

-- import Hasql.Encoders

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Functor.Contravariant ((>$<))
import Data.Morpheus.Types (GQLType (description))
import Data.Proxy (Proxy (Proxy))
import Data.String as StringUtils (IsString (fromString))
import Data.UUID
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Massalia.QueryFormat (SQLEncoder(sqlEncode))
import Massalia.SQLSelect (AQueryPart (AQueryPartConst))
import qualified Massalia.Utils as MassaliaUtils (intercalate, intercalateMap)
import Protolude

data GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType
  = GQLScalarFilter
      { isEq :: Maybe eqScalarType,
        isNotEq :: Maybe eqScalarType,
        isIn :: Maybe [eqScalarType],
        isNotIn :: Maybe [eqScalarType],
        isNull :: Maybe Bool,
        isLike :: Maybe likeScalarType,
        isIlike :: Maybe likeScalarType,
        isInSet :: Maybe (Set ordScalarType),
        isNotInSet :: Maybe (Set ordScalarType),
        isGT :: Maybe ordScalarType, -- is greater than
        isLT :: Maybe ordScalarType, -- is lesser than
        isBetween :: Maybe (ordScalarType, ordScalarType, RangeInclusivity) -- [0, 1[
      }
  deriving (Eq, Show, Generic)

deriving instance
  (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType, Ord ordScalarType) =>
  FromJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType, Ord ordScalarType) =>
  ToJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (KnownSymbol (fieldName :: Symbol), Data eqScalarType, Data likeScalarType, Data ordScalarType, Ord ordScalarType) =>
  Data (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

-- | Filter with no effect
defaultScalarFilter =
  GQLScalarFilter
    { isEq = Nothing,
      isNotEq = Nothing,
      isNull = Nothing,
      isIn = Nothing,
      isNotIn = Nothing,
      isLike = Nothing,
      isIlike = Nothing,
      isInSet = Nothing,
      isNotInSet = Nothing,
      isGT = Nothing,
      isLT = Nothing,
      isBetween = Nothing
    }

-- eq
type GQLFilterUUID (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) UUID Void Void

instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterUUID (fieldName :: Symbol)) where
  description = const $ Just "All the common operation you can think of for UUIDs"

-- eq && like
type GQLFilterText (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) Text Text Void

instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterText (fieldName :: Symbol)) where
  description = const $ Just "All the common operation you can think of for Text"

-- eq && ord
type GQLFilterInt (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) Int Void Int

instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterInt (fieldName :: Symbol)) where
  description = const $ Just "All the common operation you can think of for Int"

maybeToQueryFormat :: Monoid content => Maybe content -> content
maybeToQueryFormat Nothing = mempty
maybeToQueryFormat (Just content) = content

filterFieldToQueryPart maybePrefix maybeField = AQueryPartConst $ filterFieldToContent maybePrefix maybeField

filterFieldToMaybeQueryPart maybePrefix maybeField = AQueryPartConst <$> filterFieldToMabeContent maybePrefix maybeField

filterFieldToContent maybePrefix maybeField = maybeToQueryFormat $ filterFieldToMabeContent maybePrefix maybeField

filterFieldToMabeContent ::
  forall fieldName.
  KnownSymbol (fieldName :: Symbol) =>
  forall eqScalarType likeScalarType ordScalarType content.
  ( SQLEncoder eqScalarType content,
    SQLEncoder [eqScalarType] content,
    SQLEncoder likeScalarType content,
    SQLEncoder ordScalarType content,
    PostgresRange ordScalarType,
    Data (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)
  ) =>
  Maybe content ->
  Maybe (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType) ->
  Maybe content
filterFieldToMabeContent _ Nothing = Nothing
filterFieldToMabeContent maybeNamespace (Just filter) = case filter of
  GQLScalarFilter {isEq = (Just eqValue)} -> snippetContent actualFieldName "=" (Just eqValue)
  GQLScalarFilter {isNull = (Just True)} -> Just (prefixedFieldName <> " IS NULL")
  GQLScalarFilter
    { isIn = isInValue,
      isNotEq = isNotEqValue,
      isNotIn = isNotInValue,
      isLike = isLikeValue,
      isIlike = isIlikeValue,
      isNull = shouldFieldBeNull,
      isGT = isGTValue,
      isLT = isLTValue,
      isBetween = isBetweenValue
    } -> case ( wrappedContent prefixedFieldName "!=" isNotEqValue ""
                  <> wrappedContent prefixedFieldName "=ANY(" isInValue ")"
                  <> wrappedContent prefixedFieldName "!=ALL(" isNotInValue ")"
                  <> wrappedContent prefixedFieldName "like" isLikeValue ""
                  <> wrappedContent prefixedFieldName "ilike" isIlikeValue ""
                  <> wrappedContent prefixedFieldName ">" isGTValue ""
                  <> wrappedContent prefixedFieldName "<" isLTValue ""
                  <> []
              ) of
      [] -> Nothing
      list -> Just (MassaliaUtils.intercalate " AND " list)
  where
    prefixedFieldName = actualPrefix <> actualFieldName
    actualPrefix = maybe "" (<> ".") maybeNamespace
    actualFieldName = StringUtils.fromString (symbolVal (Proxy :: Proxy (fieldName :: Symbol)))

snippetContent ::
  forall content a. (SQLEncoder a content) =>
  content ->
  content ->
  Maybe a ->
  Maybe content
snippetContent fieldName op maybeVal = effectFunc <$> maybeVal
  where
    effectFunc parameterVal = fieldName <> " " <> op <> " " <> sqlEncode parameterVal

wrappedContent ::
  forall content.
  content ->
  ( forall filterValue. (SQLEncoder filterValue content) =>
    content ->
    Maybe filterValue ->
    content ->
    [content]
  )
wrappedContent _ _ Nothing _ = []
wrappedContent fieldName op (Just a) suffix =
  [ fieldName <> " " <> op <> " " <> sqlEncode a <> suffix
  ]

-- int4range — Range of integer
-- int8range — Range of bigint
-- numrange — Range of numeric
-- tsrange — Range of timestamp without time zone
-- tstzrange — Range of timestamp with time zone
-- daterange — Range of date
-- TODO: map the proper ranges with Hasql/haskell types
class PostgresRange a where
  -- First param is the classname
  getRangeKeyword :: b

instance PostgresRange Void where
  getRangeKeyword = panic "EA1: should never call PostgresRange for Void"

data RangeInclusivity = Inclusive | Exclusive | RightInclusive | LeftInclusive deriving (Eq, Show, Data, Generic, FromJSON, ToJSON)

-- class SQLFilter a where
--   toFilterQuery :: a -> 
