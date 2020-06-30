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
    OrderByWay(..),
    OrderingBy(..)
  )
where

import Data.Morpheus.Types (GQLType, description, KIND)
import Data.Morpheus.Kind (INPUT)
import Data.Aeson (FromJSON, ToJSON)
import Protolude

-- | A normalized API for all paginated query Args.
-- The filtered is for a simple filter,  
data Paginated filterType
  = Paginated
      { filtered :: [filterType],
        first :: Maybe Int,
        offset :: Maybe Int
      }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

defaultPaginated :: Paginated filterType
defaultPaginated = Paginated [] Nothing Nothing

instance (Typeable filterType, GQLType filterType) =>
  GQLType (Paginated filterType) where
  description = const (Just "A simple wrapper around any filter type to get pagination and OR filtered capabilities")

data OrderByWay = ASC | DESC deriving (Eq, Show, Generic, GQLType)
data OrderingBy a = OrderingBy {
  way :: OrderByWay,
  column :: a
} deriving (Eq, Show, Generic)

instance (Typeable a) => GQLType (OrderingBy a) where
  type KIND (OrderingBy a) = INPUT