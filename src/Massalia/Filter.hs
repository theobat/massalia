{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : Massalia.Filter
-- Description : A module to define an interface for simple scalar values filters.
module Massalia.Filter
  ( GQLFilterUUID,
    GQLFilterUUIDCore,
    GQLFilterText,
    GQLFilterTextCore,
    GQLFilterDay,
    GQLFilterDayCore,
    GQLFilterInt,
    GQLFilterIntCore,
    GQLFilterLocalTime,
    GQLFilterLocalTimeCore,
    GQLScalarFilter,
    GQLScalarFilterCore(..),
    FilterConstraint,
    defaultScalarFilter,
    filterFieldToMaybeContent,
    filterNamedFieldToMaybeContent,
    PostgresRange(postgresRangeName)
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
import Massalia.QueryFormat (
    SQLEncoder(
        sqlEncode,
        ignoreInGenericInstance,
        wrapEncoding
      )
  )
import Massalia.Utils (Day, LocalTime, SimpleRange(..), Inclusivity(..))
import qualified Massalia.Utils as MassaliaUtils (intercalate, intercalateMap)

import Protolude

type GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType
  = GQLScalarFilterCore eqScalarType likeScalarType ordScalarType
data GQLScalarFilterCore eqScalarType likeScalarType ordScalarType
  = GQLScalarFilter
      { isEq :: Maybe eqScalarType,
        isNotEq :: Maybe eqScalarType,
        isIn :: Maybe [eqScalarType],
        isNotIn :: Maybe [eqScalarType],
        isNull :: Maybe Bool,
        isLike :: Maybe likeScalarType,
        isIlike :: Maybe likeScalarType,
        -- The Ord class
        isGT :: Maybe ordScalarType, -- is greater than
        isLT :: Maybe ordScalarType, -- is lesser than
        isBetween :: Maybe (SimpleRange ordScalarType)
      }
  deriving (Eq, Show, Generic) -- JSON instances below

deriving instance
  (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType, Ord ordScalarType) =>
  FromJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType, Ord ordScalarType) =>
  ToJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (
    Typeable eqScalarType, Typeable likeScalarType, Typeable ordScalarType,
    GQLType eqScalarType, GQLType likeScalarType, GQLType ordScalarType
  ) =>
  GQLType (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)

-- eq
type GQLFilterUUID (fieldName :: Symbol) = GQLFilterUUIDCore
type GQLFilterUUIDCore = GQLScalarFilterCore UUID Void Void

-- eq && like
type GQLFilterText (fieldName :: Symbol) = GQLFilterTextCore
type GQLFilterTextCore = GQLScalarFilterCore Text Text Void

-- eq && ord
type GQLFilterInt (fieldName :: Symbol) = GQLFilterIntCore
type GQLFilterIntCore = GQLScalarFilterCore Int Void Int

type GQLFilterLocalTime (fieldName :: Symbol) = GQLFilterLocalTimeCore
type GQLFilterLocalTimeCore = GQLScalarFilterCore LocalTime Void LocalTime

type GQLFilterDay (fieldName :: Symbol) = GQLFilterDayCore
type GQLFilterDayCore = GQLScalarFilterCore Day Void Day

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
      isGT = Nothing,
      isLT = Nothing,
      isBetween = Nothing
    }

maybeToQueryFormat :: Monoid content => Maybe content -> content
maybeToQueryFormat Nothing = mempty
maybeToQueryFormat (Just content) = content

filterNamedFieldToMaybeContent ::
  forall fieldName. (KnownSymbol fieldName) =>
  (forall content eqScalarType likeScalarType ordScalarType.
  ( IsString content,
    SQLEncoder content eqScalarType,
    SQLEncoder content [eqScalarType],
    SQLEncoder content likeScalarType,
    SQLEncoder content ordScalarType,
    PostgresRange ordScalarType
  ) =>
  Maybe content ->
  Maybe (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType) ->
  Maybe content)
filterNamedFieldToMaybeContent a = filterFieldToMaybeContent a actualFieldName
  where actualFieldName = StringUtils.fromString (symbolVal (Proxy :: Proxy (fieldName :: Symbol)))

type FilterConstraint qf a b c = (
    SQLEncoder qf a,
    SQLEncoder qf [a],
    SQLEncoder qf b,
    SQLEncoder qf c,
    PostgresRange c
  )

filterFieldToMaybeContent ::
  ( SQLEncoder content eqScalarType,
    SQLEncoder content [eqScalarType],
    SQLEncoder content likeScalarType,
    SQLEncoder content ordScalarType,
    PostgresRange ordScalarType
  ) =>
  Maybe content ->
  content ->
  Maybe (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType) ->
  Maybe content
filterFieldToMaybeContent _ _ Nothing = Nothing
filterFieldToMaybeContent maybeNamespace actualFieldName (Just filterVal) = case filterVal of
  GQLScalarFilter {isEq = (Just eqValue)} -> snippetContent actualFieldName "=" (Just eqValue)
  GQLScalarFilter {isNull = (Just True)} -> Just (prefixedFieldName <> " IS NULL")
  GQLScalarFilter
    { isIn = isInValue,
      isNotEq = isNotEqValue,
      isNotIn = isNotInValue,
      isLike = isLikeValue,
      isIlike = isIlikeValue,
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
                  <> wrappedContent prefixedFieldName "<@" isBetweenValue ""
              ) of
      [] -> Nothing
      filtList -> Just (MassaliaUtils.intercalate " AND " filtList)
  where
    prefixedFieldName = actualPrefix <> actualFieldName
    actualPrefix = maybe "" (<> ".") maybeNamespace

snippetContent ::
  forall content a. (SQLEncoder content a) =>
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
  ( forall filterValue. (SQLEncoder content filterValue) =>
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

instance (PostgresRange a, SQLEncoder content a) => SQLEncoder content (SimpleRange a) where
  -- | test
  -- >>> (sqlEncode $ SimpleRange (Just 1::Int) Nothing Nothing) :: Text
  -- 
  sqlEncode value = postgresRangeName @a <> "(" <> startValue <> "," <> endValue <> ")"
    where
      startValue = getBoundary start
      endValue = getBoundary end
      bounds = case inclusivity value of
        Nothing -> ""
        Just II -> ", []"
        Just IE -> ", [)"
        Just EI -> ", (]"
        Just EE -> ", ()"
      getBoundary accessor = fromMaybe "null" $ (sqlEncode <$> accessor value)

class PostgresRange a where
  postgresRangeName :: (IsString textFormat) => textFormat
instance PostgresRange Int where
  postgresRangeName = "int8range"
instance PostgresRange LocalTime where
  postgresRangeName = "tsrange"
instance PostgresRange Day where
  postgresRangeName = "daterange"
instance PostgresRange Void where
  postgresRangeName = panic "in theory cannot happen, (PostgresRange Void)"

newtype ViewF filterT = ViewF filterT
newtype ExistF filterT = ExistF filterT
