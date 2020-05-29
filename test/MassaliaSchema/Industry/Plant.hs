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
    SQLDecoder(sqlDecode),
    joinEq,
    simpleEq,
    (°)
  )
import Massalia.SQLSelectStruct (
  SelectStruct(..), QueryAndDecoder(..), selectStructToListSubquery,
  queryAndDecoderToListSubquery,
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
    SQLFilter(toQueryFormatFilter),
    SelectConstraint,
    SubSelectConstraint,
    basicDecodeListSubquery,
    basicQueryAndDecoder
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
  toSelectQuery = basicQueryAndDecoder "plant"

instance (
    SubSelectConstraint queryFormat TruckFilter Truck
  ) => SQLDecoder queryFormat PlantFilter [Truck] where
  sqlDecode = basicDecodeListSubquery joinFn PlantFilter.truckList
    where
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

