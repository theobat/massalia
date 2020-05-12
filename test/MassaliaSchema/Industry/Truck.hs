{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MassaliaSchema.Industry.Truck
  ( Truck (..),
    truckInitSQL,
  )
where

import Data.Data (Data)
import Data.Morpheus.Types (GQLRootResolver (..), GQLType)
import Massalia.SelectionTree (MassaliaTree (getName, foldrChildren))
import Data.Text (Text)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic)
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( SQLEncoder (sqlEncode),
    IsString,
    FromText
  )
import Massalia.SQLSelect (RawSelectStruct (RawSelectStruct, fromPart, whereConditions), SelectStruct, getInitialValueSelect, initSelect, scalar)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import qualified MassaliaSchema.Industry.TruckFilter as TruckFilter
import Massalia.SQLClass (SQLFilter(toQueryFormatFilter))
import Protolude

data Truck
  = Truck
      { id :: UUID,
        vehicleId :: Text
      }
  deriving (Show, Generic, GQLType)

truckSelect ::
  (FromText content, MassaliaTree nodeType) =>
  nodeType -> SelectStruct Truck content -> SelectStruct Truck content
truckSelect selection = case fieldName of
  "id" -> scalarField (\e v -> e {id = v}) Decoders.uuid
  -- "vehicleId" -> simpleCol fieldName (\e v -> e{vehicleId=v}) vehicleId Decoders.text
  _ -> identity
  where
    scalarField = scalar "truck" fieldName
    fieldName = getName selection

truckInitSQL :: (
    IsString queryFormat, FromText queryFormat, Monoid queryFormat,
    MassaliaTree nodeType,
    SQLFilter queryFormat TruckFilter
  ) =>
  Maybe TruckListFilter -> nodeType -> SelectStruct Truck queryFormat
truckInitSQL filters = foldrChildren truckSelect (initialTruckQuery filters)

initialTruckQuery :: (
    Monoid queryFormat,
    IsString queryFormat,
    SQLFilter queryFormat TruckFilter
  ) => Maybe TruckListFilter -> SelectStruct Truck queryFormat
initialTruckQuery filterVal =
  getInitialValueSelect
    initSelect
      { fromPart = "truck",
        whereConditions = pure <$> toQueryPart filterVal
      }
    defaultTruck

defaultTruck = Truck nil ""

data TruckListFilter
  = TruckListFilter
      { truck :: Maybe TruckFilter
      }

toQueryPart :: (SQLFilter queryFormat TruckFilter) => Maybe TruckListFilter -> Maybe queryFormat
toQueryPart input = toQueryFormatFilter Nothing <$> truckFilterValue
  where
    truckFilterValue = join $ truck <$> input

