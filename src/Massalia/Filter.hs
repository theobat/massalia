{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
    GQLCollectionFilterEq,
    GQLScalarFilterEq,
    GQLScalarFilterOrd,
    GQLScalarFilterCore (..),
    GQLCollectionFilterCore (..),
    FilterConstraint,
    defaultScalarFilter,
    filterFieldToMaybeContent,
    filterFieldCollectionToMaybeContent,
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

-- This is for doctest only:
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

type GQLScalarFilterEq filterType =
  (GQLScalarFilterCore filterType Void Void)

type GQLScalarFilterOrd filterType =
  (GQLScalarFilterCore filterType Void filterType)

type GQLScalarFilterText filterType =
  (GQLScalarFilterCore filterType filterType Void)

type GQLCollectionFilterEq filterType =
  (GQLCollectionFilterCore filterType)

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

data GQLCollectionFilterCore eqScalarType = GQLCollectionFilterCore
  { doesContain :: Maybe [eqScalarType],
    doesNotContain :: Maybe [eqScalarType],
    isContainedBy :: Maybe [eqScalarType],
    isNotContainedBy :: Maybe [eqScalarType],
    hasLength :: Maybe Int
  }
  deriving (Eq, Show, Generic)

deriving instance
  (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType, Ord ordScalarType) =>
  FromJSON (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)

deriving instance
  (FromJSON eqScalarType) =>
  FromJSON (GQLCollectionFilterCore eqScalarType)

-- deriving newtype instance
--   (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType, Ord ordScalarType) =>
--   FromJSON (GQLScalarFilter fieldName (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType))

deriving instance
  (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType, Ord ordScalarType) =>
  ToJSON (GQLScalarFilterCore eqScalarType likeScalarType ordScalarType)

deriving instance
  (ToJSON eqScalarType) =>
  ToJSON (GQLCollectionFilterCore eqScalarType)

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
instance
  ( Typeable eqScalarType,
    GQLType eqScalarType
  ) =>
  GQLType (GQLCollectionFilterCore eqScalarType)
  where
  type KIND (GQLCollectionFilterCore eqScalarType) = INPUT

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
  forall eqScalarType likeScalarType ordScalarType.
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

-- | Takes a prefixedFieldName and a filter of type 'GQLScalarFilterCore'
-- and transforms the whole into a (maybe) query format bit.
--
-- Examples:
-- 
-- >>> -- The nothing case:
-- >>> let filter = (Just defaultScalarFilter :: Maybe GQLFilterText)
-- >>> filterFieldToMaybeContent @Text "" filter 
-- Nothing
-- >>> -- The classical case:
-- >>> filterFieldToMaybeContent @Text "foo.bar" (Just defaultScalarFilter{isEq = Just "Kanso"} :: Maybe GQLFilterText)
-- Just "foo.bar = 'Kanso'"
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
    } -> case wrappedContentInList prefixedFieldName "!=" isNotEqValue ""
                  <> wrappedContentInList prefixedFieldName "=ANY(" isInValue ")"
                  <> wrappedContentInList prefixedFieldName "!=ALL(" isNotInValue ")"
                  <> wrappedContentInList prefixedFieldName "like" isLikeValue ""
                  <> wrappedContentInList prefixedFieldName "ilike" isIlikeValue ""
                  <> wrappedContentInList prefixedFieldName ">" isGTValue ""
                  <> wrappedContentInList prefixedFieldName "<" isLTValue ""
                  <> wrappedContentInList prefixedFieldName "<@" isBetweenValue ""
                  <> maybe mempty (pure . pure $ prefixedFieldName <> " IS NOT NULL") isNullValue
              of
      [] -> Nothing
      filtList -> Just (MassaliaUtils.intercalate " AND " filtList)

-- | Takes a prefixedFieldName and a filter of type 'GQLScalarFilterCore'
-- and transforms the whole into a (maybe) query format bit.
--
-- Examples:
-- 
-- >>> -- The nothing case:
-- >>> filterFieldToMaybeContent "foo.bar" (Just defaultScalarFilter :: Maybe GQLFilterText) :: Maybe Text
-- Nothing
-- >>> filterFieldToMaybeContent "foo.bar" (Just defaultScalarFilter{isEq = Just "Kanso"} :: Maybe GQLFilterText) :: Maybe Text
-- Just "foo.bar = 'Kanso'"
--
filterFieldCollectionToMaybeContent ::
  forall qf eqScalarType.
  (QueryFormat qf, SQLEncoder [eqScalarType]) =>
  qf ->
  Maybe (GQLCollectionFilterCore eqScalarType) ->
  Maybe qf
filterFieldCollectionToMaybeContent _ Nothing = Nothing
filterFieldCollectionToMaybeContent prefixedFieldName (Just filterVal) = case filterVal of
  GQLCollectionFilterCore
    { doesContain = doesContainValue,
      doesNotContain = doesNotContainValue,
      isContainedBy = isContainedByValue,      
      isNotContainedBy = isNotContainedByValue,
      hasLength = hasLengthValue
    } -> case wrappedContentInList prefixedFieldName "@>" doesContainValue ""
                  <> wrappedContentInList ("NOT (" <> prefixedFieldName) "@>" doesNotContainValue ")"
                  <> wrappedContentInList prefixedFieldName "<@" isContainedByValue ""
                  <> wrappedContentInList ("NOT (" <> prefixedFieldName) "<@" isNotContainedByValue ")"
                  <> wrappedContentInList ("array_length(" <> prefixedFieldName <> ", 1) ") "=" hasLengthValue ""
              of
      [] -> Nothing
      filtList -> Just (MassaliaUtils.intercalate " AND " filtList)

-- | Similar to 'wrappedContent' but with an empty suffix.
-- Equals @wrappedContent fieldName op maybeVal ""@
-- Example:
-- >>> snippetContent @Text @Int "foo.bar" "=" (Just 1)
-- Just "foo.bar = 1"
snippetContent :: (QueryFormat a, SQLEncoder filterValue) => a -> a -> Maybe filterValue -> Maybe a
snippetContent fieldName op maybeVal = wrappedContent fieldName op maybeVal ""

wrappedContentInList :: (QueryFormat qf, SQLEncoder valueType) => qf -> qf -> Maybe valueType -> qf -> [qf]
wrappedContentInList fieldName op maybeVal = maybeToList . wrappedContent fieldName op maybeVal

-- | Inner function to format an sql @a op b postop@ type of expression.
-- 
-- Example:
--
-- >>> wrappedContent @Text "foo.bar" "=" (Just @Int 1) ""
-- Just "foo.bar = 1"
-- >>>
-- >>> wrappedContent @Text "foo.bar" "=ANY(" (Just @[Int] [1, 2, 3]) ")"
-- Just "foo.bar =ANY( '{3,2,1}')"
wrappedContent :: (QueryFormat qf, SQLEncoder valueType) => qf -> qf -> Maybe valueType -> qf -> Maybe qf
wrappedContent _ _ Nothing _ = Nothing
wrappedContent fieldName op (Just a) suffix = Just
  ( fieldName <> " " <> op <> " " <> sqlEncode a <> suffix
  )
