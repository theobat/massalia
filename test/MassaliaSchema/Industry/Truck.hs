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
import Data.Text (Text)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic)
import qualified Massalia.HasqlDec as Decoders
import Massalia.MorpheusTypes (Key, Selection (selectionName), ValidSelection, ValidSelectionSet)
import Massalia.QueryFormat
  ( SQLEncoder (sqlEncode),
    IsString,
    FromText
  )
import Massalia.SQLSelect (RawSelectStruct (RawSelectStruct, fromPart, whereConditions), SelectStruct, getInitialValueSelect, initSelect, scalar)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import qualified MassaliaSchema.Industry.TruckFilter as TruckFilter
import Protolude

data Truck
  = Truck
      { id :: UUID,
        vehicleId :: Text
      }
  deriving (Show, Generic, GQLType)

truckSelect ::
  (FromText content) =>
  ValidSelection -> SelectStruct Truck content -> SelectStruct Truck content
truckSelect selection = case fieldName of
  "id" -> scalarField (\e v -> e {id = v}) Decoders.uuid
  -- "vehicleId" -> simpleCol fieldName (\e v -> e{vehicleId=v}) vehicleId Decoders.text
  _ -> identity
  where
    scalarField = scalar "truck" fieldName
    fieldName = selectionName selection

truckInitSQL :: (IsString content, FromText content, Monoid content) =>
  Maybe TruckListFilter -> ValidSelectionSet -> SelectStruct Truck content
truckInitSQL filters = foldr truckSelect (initialTruckQuery filters)

initialTruckQuery :: (
    Monoid content,
    IsString content
  ) => Maybe TruckListFilter -> SelectStruct Truck content
initialTruckQuery filterVal =
  getInitialValueSelect
    initSelect
      { fromPart = "truck",
        whereConditions = toQueryPart filterVal
      }
    defaultTruck

defaultTruck = Truck nil ""

data TruckListFilter
  = TruckListFilter
      { truck :: Maybe TruckFilter
      }

toQueryPart :: Maybe filter -> Maybe content
toQueryPart Nothing = Nothing
toQueryPart (Just listFilter) = Nothing
  -- TruckFilter.toQueryPart (truck listFilter)
