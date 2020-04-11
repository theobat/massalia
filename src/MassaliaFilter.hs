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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaFilter
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
-- import qualified Hasql.DynamicStatements.Snippet as HasqlDynamic (param)

-- import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))

import Data.Data (Data, gmapQ)
import Data.Functor.Contravariant ((>$<))
import Data.Morpheus.Types (GQLType (description))
import Data.Proxy (Proxy (Proxy))
import Data.String as StringUtils (IsString (fromString))
import Data.Text (Text)
import Data.UUID
import Data.Void
import GHC.Generics (Generic)
import GHC.Generics ((:*:) ((:*:)), C1, Generic, K1 (K1), M1 (M1), Rep (Rep), from)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hasql.DynamicStatements.Snippet (Snippet)
import MassaliaQueryFormat (DefaultParamEncoder, QueryFormat (param), TextEncoder)
import MassaliaSQLSelect (AQueryPart (AQueryPartConst))
import MassaliaUtils (intercalate, intercalateMap)

data GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType
  = GQLScalarFilter
      { isEq :: Maybe eqScalarType,
        isNotEq :: Maybe eqScalarType,
        isIn :: Maybe [eqScalarType],
        isNotIn :: Maybe [eqScalarType],
        isNull :: Maybe Bool,
        isLike :: Maybe likeScalarType,
        isIlike :: Maybe likeScalarType,
        isGT :: Maybe ordScalarType, -- is greater than
        isLT :: Maybe ordScalarType, -- is lesser than
        isBetween :: Maybe (ordScalarType, ordScalarType, RangeInclusivity) -- [0, 1[
      }
  deriving (Eq, Show, Generic)

deriving instance
  (FromJSON eqScalarType, FromJSON likeScalarType, FromJSON ordScalarType) =>
  FromJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (ToJSON eqScalarType, ToJSON likeScalarType, ToJSON ordScalarType) =>
  ToJSON (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)

deriving instance
  (KnownSymbol (fieldName :: Symbol), Data eqScalarType, Data likeScalarType, Data ordScalarType) =>
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
  description = const $ Just "All the common operation you can think of for Text"

maybeToQueryFormat :: (QueryFormat content) => Maybe content -> content
maybeToQueryFormat Nothing = mempty
maybeToQueryFormat (Just content) = content

filterFieldToQueryPart maybeField = AQueryPartConst $ filterFieldToContent maybeField

filterFieldToMaybeQueryPart maybeField = AQueryPartConst <$> filterFieldToMabeContent maybeField

filterFieldToContent maybeField = maybeToQueryFormat $ filterFieldToMabeContent maybeField

filterFieldToMabeContent ::
  forall fieldName.
  KnownSymbol (fieldName :: Symbol) =>
  forall eqScalarType likeScalarType ordScalarType content.
  ( TextEncoder eqScalarType,
    DefaultParamEncoder eqScalarType,
    DefaultParamEncoder [eqScalarType],
    TextEncoder likeScalarType,
    DefaultParamEncoder likeScalarType,
    TextEncoder ordScalarType,
    DefaultParamEncoder ordScalarType,
    PostgresRange ordScalarType,
    QueryFormat content,
    Data (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType)
  ) =>
  Maybe (GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType) ->
  Maybe content
filterFieldToMabeContent Nothing = Nothing
filterFieldToMabeContent (Just filter) = case filter of
  GQLScalarFilter {isEq = (Just eqValue)} -> snippetContent actualFieldName "=" (Just eqValue)
  GQLScalarFilter {isNull = (Just True)} -> Just (StringUtils.fromString actualFieldName <> " IS NULL")
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
    } -> case ( wrappedContent actualFieldName "!=" isNotEqValue ""
                  <> wrappedContent actualFieldName "=ANY(" isInValue ")"
                  <> wrappedContent actualFieldName "!=ALL(" isNotInValue ")"
                  <> wrappedContent actualFieldName "like" isLikeValue ""
                  <> wrappedContent actualFieldName "ilike" isIlikeValue ""
                  <> wrappedContent actualFieldName ">" isGTValue ""
                  <> wrappedContent actualFieldName "<" isLTValue ""
                  <> []
              ) of
      [] -> Nothing
      list -> Just (intercalate " AND " list)
  where
    actualFieldName = symbolVal (Proxy :: Proxy (fieldName :: Symbol))

snippetContent ::
  (QueryFormat content, TextEncoder a, DefaultParamEncoder a) =>
  String ->
  content ->
  Maybe a ->
  Maybe content
snippetContent fieldName op maybeVal = effectFunc <$> maybeVal
  where
    effectFunc parameterVal = StringUtils.fromString fieldName <> " " <> op <> " " <> param parameterVal

wrappedContent ::
  (QueryFormat content, TextEncoder a, DefaultParamEncoder a) =>
  String ->
  content ->
  Maybe a ->
  content ->
  [content]
wrappedContent _ _ Nothing _ = []
wrappedContent fieldName op (Just a) suffix =
  [ StringUtils.fromString fieldName <> " " <> op <> " " <> param a <> suffix
  ]

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

data RangeInclusivity = Inclusive | Exclusive | RightInclusive | LeftInclusive deriving (Eq, Show, Data, Generic, FromJSON, ToJSON)
