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

-- |
-- Module      : Massalia.Filter
-- Description : A module to define an interface for simple scalar values filters.
module Massalia.Filter
  ( GQLFilterUUID,
    GQLFilterText,
    GQLFilterDay,
    GQLFilterInt,
    GQLFilterLocalTime,
    GQLScalarFilter (..),
    defaultScalarFilter,
    filterFieldToMabeContent,
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
import Massalia.QueryFormat (SQLEncoder(sqlEncode))
import Massalia.Utils (Day, LocalTime, SimpleRange(..), Inclusivity(..))
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

type GQLFilterLocalTime (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) LocalTime Void LocalTime
instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterLocalTime (fieldName :: Symbol))

type GQLFilterDay (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) Day Void Day
instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterDay (fieldName :: Symbol))

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

filterFieldToMabeContent ::
  forall fieldName.
  KnownSymbol (fieldName :: Symbol) =>
  forall eqScalarType likeScalarType ordScalarType content.
  ( SQLEncoder eqScalarType content,
    SQLEncoder [eqScalarType] content,
    SQLEncoder likeScalarType content,
    SQLEncoder ordScalarType content,
    PostgresRange ordScalarType
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
                  <> wrappedContent prefixedFieldName "<@" isBetweenValue ""
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

instance (PostgresRange a, SQLEncoder a content) => SQLEncoder (SimpleRange a) content where
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
