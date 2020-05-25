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

import Data.Data (Data)
import Data.Morpheus.Types (GQLRootResolver (..), GQLType)
import Massalia.SelectionTree (MassaliaTree (getName, foldrChildren))
import Data.Text (Text)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic)
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( SQLEncoder (sqlEncode),
    QueryFormat,
    IsString,
    FromText(fromText),
    BinaryQuery
  )
import Massalia.SQLSelectStruct (SelectStruct(..), QueryAndDecoder(..))
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import qualified MassaliaSchema.Industry.TruckFilter as TruckFilter
import Massalia.SQLClass (
    SQLRecord(toColumnListAndDecoder),
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    SQLFilter(toQueryFormatFilter),
    SQLRecordConfig(..)
  )
import Massalia.UtilsGQL (Paginated, defaultPaginated)
import qualified Massalia.UtilsGQL as Paginated(first, offset, filtered)
import Protolude

data Truck
  = Truck
      { id :: UUID,
        vehicleId :: Text
      }
  deriving (Show, Generic, GQLType,
    SQLRecord Text TruckFilter, SQLRecord BinaryQuery TruckFilter)

instance (
    QueryFormat queryFormat,
    SQLFilter queryFormat TruckFilter
  ) => SQLSelect queryFormat TruckFilter Truck where
  toSelectQuery opt selection filter = QueryAndDecoder {query=queryWithColumnList, decoder=decoderVal}
    where
      queryWithColumnList = rawQuery <> mempty{_select = colList}
      (colList, decoderVal) = toColumnListAndDecoder (SQLRecordConfig instanceName) selection realFilter
      realFilter = Paginated.filtered filter
      rawQuery = initialTruckQuery (fromText instanceName) filter
      instanceName = "truck"

initialTruckQuery :: (
    QueryFormat queryFormat,
    SQLFilter queryFormat TruckFilter
  ) => queryFormat -> Paginated TruckFilter -> SelectStruct queryFormat
initialTruckQuery name filterVal = mempty
      { _from = Just name,
        _where = toQueryFormatFilter Nothing <$> (Paginated.filtered filterVal)
      }

instance SQLDefault Truck where
  getDefault = defaultTruck
defaultTruck = Truck nil ""

