{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LibGenericFilter (GQLFilterUUID, GQLFilterText, GQLScalarFilter(..), defaultScalarFilter) where

import Hasql.Encoders
import Data.Functor.Contravariant ((>$<))
import Data.Text
import Data.UUID
import Data.Void

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
