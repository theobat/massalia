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

import Data.UUID (UUID, nil)
import Massalia.HasqlExec (dynamicallyParameterizedStatement)
import Massalia.SelectionTree (fromMorpheusContext)
import Massalia.MorpheusTypes
  (
    GQLType,
  )
import Massalia.QueryFormat
  ( BinaryQuery,
    SQLDecoder(sqlDecode),
    joinEq,
    simpleEq,
    (°)
  )
import Massalia.SQLSelectStruct (
  SelectStruct(..),
  queryAndDecoderToSnippetAndResult
  )
import Massalia.HasqlExec (use)
import MassaliaSchema.Industry.PlantInput (queryTest)
import MassaliaSchema.Industry.Truck (Truck)
import Protolude hiding (first)
import qualified MassaliaSchema.Industry.PlantFilter as PlantFilter
import MassaliaSchema.Industry.PlantFilter (PlantFilter, plantFilterTest)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Data.Morpheus.Types (unsafeInternalContext)
import Massalia.Utils (LocalTime, Day)
import Massalia.UtilsGQL (Paginated, defaultPaginated)
import qualified Massalia.UtilsGQL as Paginated(first, offset, filtered)
import Massalia.SQLClass (
    SQLRecord,
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    SQLRecordConfig(..),
    SQLFilter,
    SelectConstraint,
    SubSelectConstraint,
    basicDecodeListSubquery,
    basicQueryAndDecoder,
    basicEntityQuery
  )

data Plant
  = Plant
      { id :: UUID,
        name :: Text,
        createdAt :: LocalTime,
        checkDate :: Day,
        description :: Maybe Text,
        truckList :: [Truck]
      }
  deriving (Show, Generic, GQLType,
    SQLRecord Text PlantFilter, SQLRecord BinaryQuery PlantFilter)

instance (
    SelectConstraint queryFormat PlantFilter
  ) => SQLSelect queryFormat PlantFilter Plant where
  toSelectQuery = basicQueryAndDecoder (\_ -> basicEntityQuery "plant")

instance (
    SubSelectConstraint queryFormat TruckFilter Truck
  ) => SQLDecoder queryFormat PlantFilter [Truck] where
  sqlDecode = basicDecodeListSubquery contextSwitch joinFn
    where
      contextSwitch = const defaultPaginated -- PlantFilter.truckList . Paginated.filtered
      joinFn name = mempty {
        _join = [joinEq truckPlantName "truck_id" truckName "id"],
        _where = Just $ simpleEq truckPlantName "plant_id" name "id",
        _groupBy = [name ° "id"]
      }
      truckPlantName = "truck_plant"
      truckName = "truck"

instance SQLDefault Plant where
  getDefault = defaultPlant
defaultPlant = Plant {id = nil, truckList = mempty, name = ""}

plantListQuery maybePool queryArgs = do
  massaliaTree <- (pure . fromMorpheusContext) =<< unsafeInternalContext
  case maybePool of
    Nothing -> pure []
    Just pool -> lift (exec pool massaliaTree)
  where
    exec pool validSel = do
      let (snippet, result) = queryAndDecoderToSnippetAndResult $ initialSnippet validSel
      let fullSnippet = queryTest <> " " <> snippet
      let session = dynamicallyParameterizedStatement fullSnippet result
      res <- use pool session
      case res of
        Left e -> (panic $ show e)
        Right listRes -> pure listRes
    -- statement validSel = queryAndDecoderToSession $ initialSnippet validSel
    initialSnippet selSet = toSelectQuery selSet arg
    arg = defaultPaginated{Paginated.filtered = Just plantFilterTest}

