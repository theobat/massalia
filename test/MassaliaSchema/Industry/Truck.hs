{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaSchema.Industry.Truck
  ( Truck (..),
  )
where

import Data.Morpheus.Types (GQLType)
import Data.UUID (UUID, nil)
import Massalia.QueryFormat
  ( BinaryQuery
  )
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Massalia.SQLClass (
    SelectConstraint,
    SQLRecord,
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    basicQueryAndDecoder
  )
import Protolude

data Truck
  = Truck
      { id :: UUID,
        vehicleId :: Text
      }
  deriving (Show, Generic, GQLType,
    SQLRecord Text TruckFilter, SQLRecord BinaryQuery TruckFilter)

instance (
    SelectConstraint queryFormat TruckFilter
  ) => SQLSelect queryFormat TruckFilter Truck where
  toSelectQuery = basicQueryAndDecoder "truck"

instance SQLDefault Truck where
  getDefault = defaultTruck
defaultTruck :: Truck
defaultTruck = Truck nil ""

