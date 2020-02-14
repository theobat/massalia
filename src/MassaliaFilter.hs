{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaFilter (GQLFilterUUID, GQLFilterText, GQLScalarFilter(..), defaultScalarFilter) where

import Hasql.Encoders
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.UUID
import Data.Void
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as HasqlDynamic (param)
import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))
import MassaliaUtils (intercalateMap, intercalate)

data GQLScalarFilter eqScalarType likeScalarType ordScalarType = GQLScalarFilter {
  isEq :: Maybe eqScalarType,
  isNotEq :: Maybe eqScalarType,
  isIn :: Maybe [eqScalarType],
  isNotIn :: Maybe [eqScalarType],
  isLike :: Maybe likeScalarType,
  isIlike :: Maybe likeScalarType,
  isGT :: Maybe ordScalarType, -- is greater than
  isLT :: Maybe ordScalarType, -- is lesser than
  isBetween :: Maybe (ordScalarType, ordScalarType), -- [0, 1[
  isBetweenInclusive :: Maybe (ordScalarType, ordScalarType) -- [0, 1]
}

-- Filter with no effect
defaultScalarFilter = GQLScalarFilter {
  isEq = Nothing,
  isNotEq = Nothing,
  isIn = Nothing,
  isNotIn = Nothing,
  isLike = Nothing,
  isIlike = Nothing,
  isGT = Nothing,
  isLT = Nothing,
  isBetween = Nothing,
  isBetweenInclusive = Nothing
}

-- eq
type GQLFilterUUID = GQLScalarFilter UUID Void Void

-- eq && like
type GQLFilterText = GQLScalarFilter Text Text Void

instance DefaultParamEncoder Void where
  defaultParam = error "cannot call DefaultParamEncoder.defaultParam of Void"

snippetMaybe :: DefaultParamEncoder a => Snippet -> Snippet -> Maybe a -> Snippet
snippetMaybe _ _ Nothing = mempty
snippetMaybe fieldName op (Just a) = fieldName <> " " <> op <> " " <> HasqlDynamic.param a

filterFieldToSnippet ::
  DefaultParamEncoder eqScalarType =>
  DefaultParamEncoder [eqScalarType] =>
  DefaultParamEncoder likeScalarType =>
  DefaultParamEncoder ordScalarType =>
  DefaultParamEncoder (ordScalarType, ordScalarType) =>
  Snippet -> GQLScalarFilter eqScalarType likeScalarType ordScalarType -> Snippet
filterFieldToSnippet fieldName GQLScalarFilter {isEq = (Just eqValue)} = snippetMaybe fieldName "=" (Just eqValue)
filterFieldToSnippet fieldName GQLScalarFilter {
    isNotEq = isNotEqValue,
    isIn = isInValue,
    isNotIn = isNotInValue,
    isLike = isLikeValue,
    isIlike = isIlikeValue,
    isGT = isGTValue,
    isLT = isLTValue,
    isBetween = isBetweenValue,
    isBetweenInclusive = isBetweenInclusiveValue
  } = intercalate " AND " [
      snippetMaybe fieldName "!=" isNotEqValue,
      snippetMaybe fieldName "in" isInValue,
      snippetMaybe fieldName "not in" isInValue
    ]
