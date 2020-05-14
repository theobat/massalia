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

import Massalia.MorpheusTypes (GQLType)
import Data.Aeson (FromJSON, ToJSON)
import Protolude

-- | A normalized API for all paginated query Args.
data Paginated filterType
  = Paginated
      { filtered :: Maybe filterType,
        first :: Maybe Int,
        offset :: Maybe Int
      }
  deriving (Eq, Show, Generic, GQLType, FromJSON, ToJSON)

defaultPaginated :: Paginated filterType
defaultPaginated = Paginated Nothing Nothing Nothing
