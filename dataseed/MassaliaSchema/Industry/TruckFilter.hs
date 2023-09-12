{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module MassaliaSchema.Industry.TruckFilter
  ( TruckFilter (..),
    testInstance
  )
where

import qualified Data.Aeson as JSON
import Data.UUID (nil)
import Massalia.Filter
  ( GQLFilterText,
    GQLFilterUUID,
    GQLScalarFilterCore(isEq, isIn),
    defaultScalarFilter,
  )
import Massalia.QueryFormat
  ( FromText (fromText),
    MassaliaContext(..), (°)
  )
import Massalia.SQLClass (
    SQLFilter,
    SQLFilterField(filterStruct), existsOrNotPrimitive
  )
import Data.Morpheus.Types (GQLType(description))
import Protolude
import Massalia.SQLSelectStruct (SelectStruct(..))

data TruckFilter
  = TruckFilter
      { id :: Maybe GQLFilterUUID,
        vehicleId :: Maybe GQLFilterText
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON, SQLFilter)


instance SQLFilterField TruckFilter where
  filterStruct opts selection val = case selection of
    "exists_truck" -> Just $ handleExistFilter True opts selection val
    "not_exists_truck" -> Just $ handleExistFilter False opts selection val
    _ -> Nothing
    where
      handleExistFilter isExist = existsOrNotPrimitive isExist filterInside actualFilter
      filterInside = True
      actualFilter fatherTableName =
        ( mempty
            { _select = pure "1",
              _from = Just "truck",
              _where = Just condition
            },
          "exists_subquery_name"
        )
        where
          condition = ("truck" ° "plant_id") <> "=" <> (ftn ° "id")
          ftn = fromText fatherTableName

testInstance :: TruckFilter
testInstance =
  TruckFilter
    { id = pure $ defaultScalarFilter {isEq = Just nil},
      vehicleId = Nothing
    }

instance GQLType TruckFilter where
  description = const $ Just ("A set of filters for the Truck type" :: Text)

instance MassaliaContext TruckFilter where
  getDecodeOption = const mempty
  setDecodeOption = const identity