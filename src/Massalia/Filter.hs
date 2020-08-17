{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
    GQLFilterZonedTime,
    GQLFilterZonedTimeEq,
    GQLScalarFilterCore (..),
    FilterConstraint,
    defaultScalarFilter,
    filterFieldToMaybeContent,
    PostgresRange (postgresRangeName),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types (GQLType, KIND)
import Data.UUID ( UUID )
import Massalia.QueryFormat
  ( PostgresRange (postgresRangeName),
    QueryFormat (sqlEncode),
    SQLEncoder,
  )
import Massalia.Utils
  ( Day,
    LocalTime,
    SimpleRange (..),
    ZonedTime,
    ZonedTimeEq,
  )
import qualified Massalia.Utils as MassaliaUtils (intercalate)
import Protolude

type GQLScalarFilterEq filterType =
  (GQLScalarFilterCore filterType Void Void)

type GQLScalarFilterOrd filterType =
  (GQLScalarFilterCore filterType Void filterType)

type GQLScalarFilterText filterType =
  (GQLScalarFilterCore filterType filterType Void)

data GQLScalarFilterCore eqScalarType likeScalarType ordScalarType = GQLScalarFilter
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
  FromJSON (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)

-- deriving newtype instance
--   (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType, Ord ordScalarType) =>
--   FromJSON (GQLScalarFilter fieldName (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType))

deriving instance
  (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType, Ord ordScalarType) =>
  ToJSON (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)

-- deriving newtype instance
--   (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType, Ord ordScalarType) =>
--   ToJSON (GQLScalarFilter fieldName (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType))

instance
  ( Typeable eqScalarType,
    Typeable likeScalarType,
    Typeable ordScalarType,
    GQLType eqScalarType,
    GQLType likeScalarType,
    GQLType ordScalarType
  ) =>
  GQLType (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)
  where
  type KIND (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType) = INPUT

-- deriving via (NamedFilter (Maybe (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType))) instance
--     (
--       KnownSymbol fieldName,
--       Typeable eqScalarType, Typeable likeScalarType, Typeable ordScalarType,
--       GQLType eqScalarType, GQLType likeScalarType, GQLType ordScalarType
--   ) => GQLType (GQLScalarFilter fieldName (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType))
-- type CUSTOM (GQLScalarFilter fieldName (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)) = 'False

-- eq
type GQLFilterUUID = GQLScalarFilterEq UUID

-- eq && like
type GQLFilterText = GQLScalarFilterText Text

-- eq && ord
type GQLFilterInt = GQLScalarFilterOrd Int

type GQLFilterLocalTime = GQLScalarFilterOrd LocalTime

type GQLFilterZonedTime = GQLScalarFilterOrd ZonedTime

type GQLFilterZonedTimeEq = GQLScalarFilterOrd ZonedTimeEq

type GQLFilterDay = GQLScalarFilterOrd Day

-- | Filter with no effect
-- defaultScalarFilter :: GQLScalarFilterCore eqScalarType likeScalarType ordScalarType
defaultScalarFilter ::
  GQLScalarFilterCore
    eqScalarType
    likeScalarType
    ordScalarType
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

type FilterConstraint a b c =
  ( SQLEncoder a,
    SQLEncoder [a],
    Show a,
    SQLEncoder b,
    SQLEncoder c,
    PostgresRange c
    -- QueryFormat qf
  )

filterFieldToMaybeContent ::
  forall qf eqScalarType likeScalarType ordScalarType.
  (QueryFormat qf, FilterConstraint eqScalarType likeScalarType ordScalarType) =>
  qf ->
  Maybe (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType) ->
  Maybe qf
filterFieldToMaybeContent _ Nothing = Nothing
filterFieldToMaybeContent prefixedFieldName (Just filterVal) = case filterVal of
  GQLScalarFilter {isEq = (Just eqValue)} -> snippetContent prefixedFieldName "=" (Just eqValue)
  GQLScalarFilter {isNull = (Just True)} -> Just (prefixedFieldName <> " IS NULL")
  GQLScalarFilter
    { isIn = isInValue,
      isNotEq = isNotEqValue,
      isNotIn = isNotInValue,
      isLike = isLikeValue,
      isIlike = isIlikeValue,
      isGT = isGTValue,
      isLT = isLTValue,
      isBetween = isBetweenValue,
      isNull = isNullValue
    } -> case ( wrappedContent prefixedFieldName "!=" isNotEqValue ""
                  <> wrappedContent prefixedFieldName "=ANY(" isInValue ")"
                  <> wrappedContent prefixedFieldName "!=ALL(" isNotInValue ")"
                  <> wrappedContent prefixedFieldName "like" isLikeValue ""
                  <> wrappedContent prefixedFieldName "ilike" isIlikeValue ""
                  <> wrappedContent prefixedFieldName ">" isGTValue ""
                  <> wrappedContent prefixedFieldName "<" isLTValue ""
                  <> wrappedContent prefixedFieldName "<@" isBetweenValue ""
                  <> fromMaybe mempty (((pure . pure) $ prefixedFieldName <> " IS NOT NULL") <$> isNullValue)
              ) of
      [] -> Nothing
      filtList -> Just (MassaliaUtils.intercalate " AND " filtList)

snippetContent ::
  forall qf a.
  (SQLEncoder a, QueryFormat qf) =>
  qf ->
  qf ->
  Maybe a ->
  Maybe qf
snippetContent fieldName op maybeVal = effectFunc <$> maybeVal
  where
    effectFunc parameterVal = fieldName <> " " <> op <> " " <> sqlEncode parameterVal

wrappedContent ::
  forall qf.
  (QueryFormat qf) =>
  qf ->
  ( forall filterValue.
    (SQLEncoder filterValue) =>
    qf ->
    Maybe filterValue ->
    qf ->
    [qf]
  )
wrappedContent _ _ Nothing _ = []
wrappedContent fieldName op (Just a) suffix =
  [ fieldName <> " " <> op <> " " <> sqlEncode a <> suffix
  ]
