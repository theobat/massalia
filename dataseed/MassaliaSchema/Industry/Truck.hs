{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PartialTypeSignatures #-}

module MassaliaSchema.Industry.Truck
  ( Truck (..),
  )
where

import Data.Morpheus.Types (GQLType)
import Data.UUID (UUID, nil)
import Massalia.QueryFormat
  ( BinaryQuery,
    QueryFormat,
    SQLDecoder,
    SQLEncoder,
    MassaliaContext(..)
  )
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Massalia.SQLClass (
    SQLRecord,
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    basicQueryAndDecoder,
    basicEntityNoFilter,
  )
import Protolude
import Massalia.UtilsGQL (Paginated, defaultPaginated)

data Truck
  = Truck
      { id :: UUID,
        vehicleId :: Text
      }
  deriving (Show, Generic, Eq, GQLType)

deriving instance SQLRecord (Paginated TruckFilter) Truck

-- | TODO: add actual truckFilter application
instance SQLSelect (Paginated TruckFilter) Truck where
  toSelectQuery = basicQueryAndDecoder (basicEntityNoFilter "truck")

instance SQLDefault Truck where
  getDefault = defaultTruck
defaultTruck :: Truck
defaultTruck = Truck nil ""

newtype Test = Test UUID deriving (Eq, Show)
-- deriving via UUIDWrapper instance (QueryFormat qf) => SQLDecoder qf filterType Test

instance MassaliaContext (Paginated t) where
  getDecodeOption = const mempty
  setDecodeOption = const identity