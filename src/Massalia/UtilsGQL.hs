{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Massalia.UtilsGQL
-- Description : A standard interface for query arguments (including pagination).
module Massalia.UtilsGQL
  ( -- TEXT
    Paginated(..),
    defaultPaginated,
    OrderByWay (..),
    OrderingBy (..),
    NullsOrder (..),
  )
where

import Data.Morpheus.Types (GQLType)
import Data.Aeson (FromJSON, ToJSON)
import Protolude

-- | A normalized API for all paginated query Args.
-- The filtered is for a simple filter,
data Paginated filterType = Paginated
  { -- | A filter applied __before__ globalFilter.
    unionFilter :: Maybe [filterType],
-- | A filter applied __before__ globalFilter.
    unionFilterPaginated :: Maybe [Paginated filterType],
    -- | A filter applied __around__ the union of unionFilter effects
    globalFilter :: Maybe filterType,
    -- | The limit in the query
    first :: Maybe Int,
    -- | The offset in the query
    offset :: Maybe Int
  }
  deriving (Eq, Show, Generic, GQLType, FromJSON, ToJSON)

defaultPaginated :: Paginated filterType
defaultPaginated = Paginated Nothing Nothing Nothing Nothing Nothing

data OrderByWay = ASC | DESC deriving (Eq, Show, Generic, GQLType)
-- | Nulls first or nulls last.
data NullsOrder = NFirst | NLast deriving (Eq, Show, Generic, GQLType)
data OrderingBy a = OrderingBy
  { way :: OrderByWay,
    column :: a,
    nullsOrd :: Maybe NullsOrder
  }
  deriving (Eq, Show, Generic, GQLType)
