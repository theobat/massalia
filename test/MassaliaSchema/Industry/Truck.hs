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
    HasqlSnippet
  )
import Massalia.SQLRawSelect (addSelectColumns)
import Massalia.SQLSelect (SelectStruct(SelectStruct), RawSelectStruct (RawSelectStruct, fromPart, whereConditions), SelectStruct, getInitialValueSelect, initSelect, scalar)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import qualified MassaliaSchema.Industry.TruckFilter as TruckFilter
import Massalia.SQLClass (
    SQLColumn(toColumnListAndDecoder),
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    SQLFilter(toQueryFormatFilter),
    SQLColumnConfig(..)
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
    SQLColumn Text TruckFilter, SQLColumn HasqlSnippet TruckFilter)

instance (
    QueryFormat queryFormat,
    SQLFilter queryFormat TruckFilter
  ) => SQLSelect queryFormat TruckFilter Truck where
  toSelectQuery opt selection filter = SelectStruct queryWithColumnList decoder
    where
      queryWithColumnList = addSelectColumns (pure <$> colList) [] rawQuery
      (colList, decoder) = toColumnListAndDecoder (SQLColumnConfig instanceName) selection realFilter
      realFilter = Paginated.filtered filter
      rawQuery = initialTruckQuery (fromText instanceName) filter
      instanceName = "truck"

initialTruckQuery :: (
    IsString queryFormat,
    SQLFilter queryFormat TruckFilter
  ) => queryFormat -> Paginated TruckFilter -> RawSelectStruct queryFormat
initialTruckQuery name filterVal = initSelect
      { fromPart = pure name,
        whereConditions = pure <$> toQueryFormatFilter Nothing <$> (Paginated.filtered filterVal)
      }

instance SQLDefault Truck where
  getDefault = defaultTruck
defaultTruck = Truck nil ""

