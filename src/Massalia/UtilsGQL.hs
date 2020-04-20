{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.UtilsGQL
-- Description : A standard interface for query arguments (including pagination).
module Massalia.UtilsGQL
  ( -- TEXT
    QueryArgs (..),
    QueryArgsPaginated (..),
  )
where

import Massalia.MorpheusTypes (GQLType)
import Protolude

-- | A normalized API for all (non paginated) query Args.
data QueryArgs filtered
  = QueryArgs
      { filtered :: filtered
      }
  deriving (Generic, GQLType)

-- | A normalized API for all paginated query Args.
data QueryArgsPaginated filtered
  = QueryArgsPaginated
      { filtered :: filtered,
        first :: Int,
        offset :: Int
      }
  deriving (Generic, GQLType)
