{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.UtilsGQL
-- Description : A standard interface for query arguments (including pagination).
module Massalia.UtilsGQL
  ( -- TEXT
    Paginated (..),
    defaultPaginated
  )
where

import Massalia.MorpheusTypes (GQLType, description)
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