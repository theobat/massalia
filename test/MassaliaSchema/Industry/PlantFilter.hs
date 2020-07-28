{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module MassaliaSchema.Industry.PlantFilter
  ( PlantFilter (..),
    plantFilterTest
  )
where

import qualified Data.Aeson as JSON
import Massalia.Utils (
    SimpleRange(..),
    defaultSimpleRange
  )
import Massalia.Filter
  ( GQLFilterText,
    GQLFilterUUID,
    GQLFilterDay,
    GQLScalarFilterCore (isBetween),
    defaultScalarFilter,
  )
import MassaliaSchema.Industry.TruckFilter (TruckFilter, testInstance)
import Massalia.QueryFormat
  ( BinaryQuery,
    SQLEncoder(..),
    QueryFormat,
    MassaliaContext(..)
  )
import Massalia.SQLClass (SQLFilter)
import Protolude
import Data.Morpheus.Types (GQLType(description), KIND)
import Data.Morpheus.Kind (INPUT)
import Massalia.UtilsGQL (Paginated, defaultPaginated, globalFilter)

data PlantFilter
  = PlantFilter
      { id :: Maybe GQLFilterUUID,
        name :: Maybe GQLFilterText,
        checkDate :: Maybe GQLFilterDay,
        truckList :: Maybe (Paginated TruckFilter),
        existsTruck :: Maybe TruckFilter
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON,
    SQLFilter)

instance GQLType PlantFilter where
  type KIND PlantFilter = INPUT
  description = const $ Just ("A set of filters for the Plant type" :: Text)
  

plantFilterTest :: PlantFilter
plantFilterTest = 
  PlantFilter
    { id = Nothing,
      name = Nothing,
      checkDate = pure filter,
      truckList = Just defaultPaginated{globalFilter = pure testInstance},
      existsTruck = Just testInstance
    }
  where filter = defaultScalarFilter {
                  isBetween = Just defaultSimpleRange{
                    start = case (JSON.eitherDecode "\"1991-08-21\"") of
                      Left _ -> Nothing
                      Right a -> Just a
                  }
                }


instance MassaliaContext PlantFilter where
  getDecodeOption = const mempty
  setDecodeOption = const identity
-- (getDecodeOption, setDecodeOption)
