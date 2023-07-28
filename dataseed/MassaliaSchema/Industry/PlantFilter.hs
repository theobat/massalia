{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module MassaliaSchema.Industry.PlantFilter
  ( PlantFilter (..),
    plantFilterTest,
  )
where

import qualified Data.Aeson as JSON
import Data.Morpheus.Types (GQLType(..), directives, typeDirective, Describe (Describe))
import Massalia.Filter
  ( GQLFilterDay,
    GQLFilterText,
    GQLFilterUUID,
    GQLScalarFilterCore (isBetween, isNotEq),
    defaultScalarFilter,
  )
import Massalia.QueryFormat
  ( MassaliaContext (..),
    SQLDecoder (..),
    joinEq,
    simpleEq,
    (°),
  )
import Massalia.SQLClass
import Massalia.SQLSelectStruct
  ( SelectStruct (_groupBy, _join, _where),
  )
import Massalia.Utils
  ( SimpleRange (..),
    defaultSimpleRange,
  )
import Massalia.UtilsGQL (Paginated, defaultPaginated, globalFilter)
import MassaliaSchema.Industry.Truck (Truck)
import MassaliaSchema.Industry.TruckFilter (TruckFilter, testInstance)
import Protolude

data PlantFilter = PlantFilter
  { id :: Maybe GQLFilterUUID,
    name :: Maybe GQLFilterText,
    checkDate :: Maybe GQLFilterDay,
    truckList :: Maybe (Paginated TruckFilter),
    existsTruck :: Maybe TruckFilter
  }
  deriving
    ( Show,
      Generic,
      JSON.FromJSON,
      JSON.ToJSON,
      SQLFilter
    )

instance GQLType PlantFilter where
  directives _ = typeDirective (Describe "A set of filters for the Plant type")

instance SQLDecoder (Paginated PlantFilter) [Truck] where
  sqlExpr = basicDecodeListSubquery contextSwitch joinFn
    where
      contextSwitch _ = undefined :: (Paginated TruckFilter)
      joinFn plantSqlName =
        mempty
          { _join = [joinEq truckPlantName "truck_id" truckName "id"],
            _where = Just $ simpleEq truckPlantName "plant_id" plantSqlName "id",
            _groupBy = [plantSqlName ° "id"]
          }
      truckPlantName = "truck_plant"
      truckName = "truck"
  sqlDecoder = decoderFromSQLDefault

plantFilterTest :: PlantFilter
plantFilterTest =
  PlantFilter
    { id = Nothing,
      name = Nothing,
      checkDate = pure filterVal,
      truckList = Just defaultPaginated {globalFilter = pure testInstance},
      existsTruck = Just testInstance
    }
  where
    filterVal =
      defaultScalarFilter
        { isNotEq = exampleValue,
          isBetween =
            Just
              defaultSimpleRange
                { start = exampleValue,
                  end = exampleValue
                }
        }
    exampleValue = case JSON.eitherDecode "\"1991-08-21\"" of
      Left _ -> Nothing
      Right a -> Just a

instance MassaliaContext PlantFilter where
  getDecodeOption = const mempty
  setDecodeOption = const identity
