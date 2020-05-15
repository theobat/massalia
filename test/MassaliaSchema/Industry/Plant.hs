{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaSchema.Industry.Plant
  ( Plant (..),
    plantListQuery,
  )
where

import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.UUID (UUID, nil)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import qualified Hasql.Connection as Connection
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Massalia.HasqlDec as Decoders
import Massalia.HasqlExec (dynamicallyParameterizedStatement)
import Massalia.SelectionTree (MassaliaTree (getName, foldrChildren), fromMorpheusContext)
import Massalia.MorpheusTypes
  (
    GQLRootResolver (..),
    GQLType,
  )
import Massalia.QueryFormat
  ( BinaryQuery,
    SQLEncoder,
    DefaultParamEncoder,
    FromText(fromText),
    QueryFormat,
    SQLEncoder(sqlEncode),
    SQLDecoder(sqlDecode)
  )
import Massalia.SQLSelectStruct (
  SelectStruct(..), QueryAndDecoder(..), selectStructToListSubquery,
  queryAndDecoderToListSubquery,
  queryAndDecoderToSnippetAndResult
  )
import MassaliaSchema.Industry.PlantInput (queryTest)
import MassaliaSchema.Industry.Truck (Truck)
import Protolude hiding (first)
import Text.Pretty.Simple (pPrint)
import Massalia.HasqlExec (Pool, use)
import qualified MassaliaSchema.Industry.PlantFilter as PlantFilter
import MassaliaSchema.Industry.PlantFilter (PlantFilter, plantFilterTest)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Massalia.SQLClass (SQLFilter(toQueryFormatFilter))
import Data.Morpheus.Types (unsafeInternalContext)
import Massalia.Utils (LocalTime, Day)
import Massalia.UtilsGQL (Paginated, defaultPaginated)
import qualified Massalia.UtilsGQL as Paginated(first, offset, filtered)
import Massalia.SQLClass (
    SQLColumn(toColumnListAndDecoder),
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    SQLFilter(toQueryFormatFilter),
    SQLColumnConfig(..)
  )

data Plant
  = Plant
      { id :: UUID,
        name :: Text,
        createdAt :: LocalTime,
        checkDate :: Day,
        truckList :: [Truck]
      }
  deriving (Show, Generic, GQLType,
    SQLColumn Text PlantFilter, SQLColumn BinaryQuery PlantFilter)

instance (
    QueryFormat queryFormat,
    SQLEncoder Int queryFormat,
    SQLFilter queryFormat PlantFilter
  ) => SQLSelect queryFormat PlantFilter Plant where
  toSelectQuery opt selection filter = QueryAndDecoder {query=queryWithColumnList, decoder=decoderVal}
    where
      queryWithColumnList = rawQuery <> mempty{_select = colList}
      (colList, decoderVal) = toColumnListAndDecoder (SQLColumnConfig instanceName) selection realFilter
      realFilter = Paginated.filtered filter
      rawQuery = initialPlantQuery (fromText $ instanceName) filter
      instanceName = "plant"

instance (
    QueryFormat queryFormat,
    SQLFilter queryFormat TruckFilter,
    SQLColumn queryFormat TruckFilter Truck
  ) => SQLDecoder queryFormat PlantFilter [Truck] where
  sqlDecode filterParent selection = (const qer, dec)
    where
      (qer, dec) = queryAndDecoderToListSubquery queryUpdated
      queryUpdated = subQueryRaw{query = query subQueryRaw <> mempty {
        _join = ["JOIN truck_plant ON truck.id=truck_plant.truck_id"],
        _where = Just ("truck_plant.plant_id=" <> "plant" <> ".id"),
        _groupBy = ["plant.id"]
      }}
      subQueryRaw = toSelectQuery Nothing selection filterChild
      filterChild = fromMaybe defaultPaginated (join $ PlantFilter.truckList <$> filterParent)

initialPlantQuery :: (
    SQLFilter queryFormat PlantFilter,
    SQLEncoder Int queryFormat,
    Monoid queryFormat, IsString queryFormat
  ) =>
  queryFormat -> Paginated PlantFilter -> SelectStruct queryFormat
initialPlantQuery name filter = mempty
      { _from = Just name,
        _where = toQueryFormatFilter Nothing <$> (Paginated.filtered filter),
        _offsetLimit = Just (sqlEncode <$> Paginated.offset filter, sqlEncode $ fromMaybe 10000 $ Paginated.first filter)
      }

instance SQLDefault Plant where
  getDefault = defaultPlant
defaultPlant = Plant {id = nil, truckList = mempty, name = ""}

plantListQuery pool queryArgs = do
  massaliaTree <- (pure . fromMorpheusContext) =<< unsafeInternalContext
  lift (exec massaliaTree)
  where
    exec validSel = do
      let (snippet, result) = queryAndDecoderToSnippetAndResult $ initialSnippet validSel
      let fullSnippet = queryTest <> " " <> snippet
      let session = dynamicallyParameterizedStatement fullSnippet result
      res <- use pool session
      case res of
        Left e -> (panic $ show e)
        Right listRes -> pure listRes
    -- statement validSel = queryAndDecoderToSession $ initialSnippet validSel
    initialSnippet selSet = toSelectQuery Nothing selSet arg
    arg = defaultPaginated{Paginated.filtered = Just plantFilterTest}

