{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MassaliaFilter (
  GQLFilterUUID,
  GQLFilterText,
  GQLScalarFilter(..),
  defaultScalarFilter,
  filterFieldToSnippet
) where

import Hasql.Encoders
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.UUID
import Data.Void
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as HasqlDynamic (param)
import Data.String as StringUtils (IsString(fromString))
import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))
import MassaliaUtils (intercalateMap, intercalate)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

data GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType = GQLScalarFilter {
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
type GQLFilterUUID fieldName = GQLScalarFilter fieldName UUID Void Void
-- eq && like
type GQLFilterText fieldName = GQLScalarFilter fieldName Text Text Void

instance DefaultParamEncoder Void where
  defaultParam = error "cannot call DefaultParamEncoder.defaultParam of Void"

snippetMaybe :: DefaultParamEncoder a => String -> Snippet -> Maybe a -> Snippet
snippetMaybe _ _ Nothing = mempty
snippetMaybe fieldName op (Just a) = (StringUtils.fromString fieldName) <> " " <> op <> " " <> HasqlDynamic.param a

filterFieldToSnippet ::
  forall fieldName. KnownSymbol (fieldName :: Symbol) =>
  forall eqScalarType likeScalarType ordScalarType.
    ( DefaultParamEncoder eqScalarType,
      DefaultParamEncoder [eqScalarType],
      DefaultParamEncoder likeScalarType,
      DefaultParamEncoder ordScalarType,
      DefaultParamEncoder (ordScalarType, ordScalarType)) =>
  GQLScalarFilter (fieldName :: Symbol) eqScalarType likeScalarType ordScalarType -> Snippet
filterFieldToSnippet filter = case filter of
  GQLScalarFilter {isEq = (Just eqValue)} -> snippetMaybe actualFieldName "=" (Just eqValue)
  where
    actualFieldName = symbolVal $ (Proxy :: Proxy (fieldName :: Symbol))
    -- fieldNameToString = symbolVal :: (Proxy (fieldName :: Symbol) -> String)

-- filterFieldToSnippet fieldName 
-- filterFieldToSnippet fieldName GQLScalarFilter {
--     isNotEq = isNotEqValue,
--     isIn = isInValue,
--     isNotIn = isNotInValue,
--     isLike = isLikeValue,
--     isIlike = isIlikeValue,
--     isGT = isGTValue,
--     isLT = isLTValue,
--     isBetween = isBetweenValue,
--     isBetweenInclusive = isBetweenInclusiveValue
--   } = intercalate " AND " [
--       snippetMaybe fieldName "!=" isNotEqValue,
--       snippetMaybe fieldName "in" isInValue,
--       snippetMaybe fieldName "not in" isInValue
--     ]
