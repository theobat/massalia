{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Massalia.UtilsGQL
-- Description : A standard interface for query arguments (including pagination).
module Massalia.UtilsGQL
  ( -- TEXT
    Paginated (..),
    defaultPaginated,
    OrderByWay (..),
    OrderingBy (..),
  )
where

import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types (GQLType, KIND, description)
import Data.Aeson (FromJSON, ToJSON)
import Protolude

-- | A normalized API for all paginated query Args.
-- The filtered is for a simple filter,
data Paginated filterType = Paginated
  { -- | A filter applied __before__ globalFilter.
    unionFilter :: Maybe [filterType],
    -- | A filter applied __around__ the union of unionFilter effects
    globalFilter :: Maybe filterType,
    -- | The limit in the query
    first :: Maybe Int,
    -- | The offset in the query
    offset :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

defaultPaginated :: Paginated filterType
defaultPaginated = Paginated Nothing Nothing Nothing Nothing

instance
  (Typeable filterType, GQLType filterType) =>
  GQLType (Paginated filterType)
  where
  description = const (Just "A simple wrapper around any filter type to get pagination and OR filtered capabilities")

data OrderByWay = ASC | DESC deriving (Eq, Show, Generic, GQLType)
data OrderingBy a = OrderingBy
  { way :: OrderByWay,
    column :: a
  }
  deriving (Eq, Show, Generic)


instance (Typeable a) => GQLType (OrderingBy a) where
  type KIND (OrderingBy a) = INPUT