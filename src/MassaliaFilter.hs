{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MassaliaFilter (
  GQLFilterUUID,
  GQLFilterText,
  GQLScalarFilter(..),
  defaultScalarFilter,
  filterFieldToContent,
  filterFieldToQueryPart
) where

import Hasql.Encoders
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.UUID
import Data.Void
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as HasqlDynamic (param)
import Data.String as StringUtils (IsString(fromString))
import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))
import MassaliaUtils (intercalateMap, intercalate)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Morpheus.Types (GQLType(description))
import MassaliaEncoder (DynamicParameters(param), TextEncoder, DefaultParamEncoder)
import MassaliaSQLSelect (ASelectQueryPart(SelectQueryPart))

data GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType = GQLScalarFilter {
  isEq :: Maybe eqScalarType,
  isNotEq :: Maybe eqScalarType,
  isIn :: Maybe [eqScalarType],
  isNotIn :: Maybe [eqScalarType],
  isNull :: Maybe Bool,
  isLike :: Maybe likeScalarType,
  isIlike :: Maybe likeScalarType,
  isGT :: Maybe ordScalarType, -- is greater than
  isLT :: Maybe ordScalarType, -- is lesser than
  isBetween :: Maybe (ordScalarType, ordScalarType, RangeInclusivity) -- [0, 1[
} deriving (Eq, Show, Generic)

deriving instance
  (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType)
  => FromJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType) =>
  ToJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

-- Filter with no effect
defaultScalarFilter = GQLScalarFilter {
  isEq = Nothing,
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

-- eq
type GQLFilterUUID (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) UUID Void Void
instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterUUID (fieldName :: Symbol)) where
  description = const $ Just "All the common operation you can think of for UUIDs"

-- eq && like
type GQLFilterText (fieldName :: Symbol) = GQLScalarFilter (fieldName :: Symbol) Text Text Void
instance (KnownSymbol (fieldName :: Symbol)) => GQLType (GQLFilterText (fieldName :: Symbol)) where
  description = const $ Just "All the common operation you can think of for Text"

instance DefaultParamEncoder Void where
  defaultParam = error "cannot call DefaultParamEncoder.defaultParam of Void"

filterFieldToQueryPart maybeField = SelectQueryPart $ filterFieldToContent maybeField

filterFieldToContent ::
  forall fieldName. KnownSymbol (fieldName :: Symbol) =>
  forall eqScalarType likeScalarType ordScalarType content.
    ( TextEncoder eqScalarType, DefaultParamEncoder eqScalarType,
      DefaultParamEncoder [eqScalarType],
      TextEncoder likeScalarType, DefaultParamEncoder likeScalarType,
      TextEncoder ordScalarType, DefaultParamEncoder ordScalarType,
      PostgresRange ordScalarType,
      Monoid content, IsString content, DynamicParameters content
    ) =>
  Maybe (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType) -> content
filterFieldToContent Nothing = mempty
filterFieldToContent (Just filter) = case filter of
  GQLScalarFilter {isEq = (Just eqValue)} -> snippetContent actualFieldName "=" (Just eqValue)
  GQLScalarFilter {isNull = (Just True)} -> StringUtils.fromString actualFieldName <> " IS NULL"
  GQLScalarFilter {
    isNotEq = isNotEqValue,
    isIn = isInValue,
    isNotIn = isNotInValue,
    isLike = isLikeValue,
    isIlike = isIlikeValue,
    isNull = shouldFieldBeNull,
    isGT = isGTValue,
    isLT = isLTValue,
    isBetween = isBetweenValue
  } -> intercalate " AND " (
      wrappedContent actualFieldName "!=" isNotEqValue "" <>
      wrappedContent actualFieldName "=ANY(" isInValue ")" <>
      wrappedContent actualFieldName "!=ALL(" isNotInValue ")" <>
      wrappedContent actualFieldName "like" isLikeValue "" <>
      wrappedContent actualFieldName "ilike" isIlikeValue ""
      -- scalarFieldOrd ">" isGTValue "" <>
      -- scalarFieldOrd "<" isLTValue "" <>
      -- betweenAsSnippetList actualFieldName isBetweenValue
    )
  where
    -- scalarFieldEq = wrappedContent actualFieldName
    -- scalarFieldLike = wrappedContent actualFieldName
    -- scalarFieldOrd = wrappedContent actualFieldName
    -- listField = wrappedContent actualFieldName
    actualFieldName = symbolVal (Proxy :: Proxy (fieldName :: Symbol))

snippetContent ::
  (DynamicParameters content, Monoid content, IsString content,
  TextEncoder a, DefaultParamEncoder a) =>
  String -> content -> Maybe a -> content
snippetContent _ _ Nothing = mempty
snippetContent fieldName op (Just a) = StringUtils.fromString fieldName <> " " <> op <> " " <> MassaliaEncoder.param a

wrappedContent :: 
  (DynamicParameters content, Monoid content, IsString content,
  TextEncoder a, DefaultParamEncoder a) =>
  String -> content -> Maybe a -> content -> [content]
wrappedContent _ _ Nothing _ = []
wrappedContent fieldName op (Just a) suffix = [
    StringUtils.fromString fieldName <> " " <> op <> " " <> MassaliaEncoder.param a <> suffix
  ]

-- betweenAsSnippetList :: forall a. (PostgresRange a, DefaultParamEncoder a) => String -> Maybe (a, a, RangeInclusivity) -> [Snippet] 
-- betweenAsSnippetList _ Nothing = []
-- betweenAsSnippetList fieldName (Just (lower, upper, _)) = [
--     StringUtils.fromString fieldName <> " <@ " <> rangePart
--   ]
--   where rangePart = StringUtils.fromString (getRangeKeyword @a) <> "(" <> HasqlDynamic.param lower <> ", " <> HasqlDynamic.param upper <> ")" 
  


-- int4range — Range of integer
-- int8range — Range of bigint
-- numrange — Range of numeric
-- tsrange — Range of timestamp without time zone
-- tstzrange — Range of timestamp with time zone
-- daterange — Range of date
-- TODO: map the proper ranges with Hasql/haskell types
class DefaultParamEncoder a => PostgresRange a where
  -- First param is the classname
  getRangeKeyword :: String

instance PostgresRange Void where
  getRangeKeyword = error "EA1: should never call PostgresRange for Void"


data RangeInclusivity = Inclusive | Exclusive | RightInclusive | LeftInclusive deriving (Eq, Show, Generic, FromJSON, ToJSON)
