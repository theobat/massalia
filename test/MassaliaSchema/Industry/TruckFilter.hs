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

module MassaliaSchema.Industry.TruckFilter
  ( TruckFilter (..),
    testInstance
  )
where

import qualified Data.Aeson as JSON
import Data.Data (Data, gmapQ)
import Data.Text (Text)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic, from)
import Massalia.Filter
  ( GQLFilterText,
    GQLFilterUUID,
    GQLScalarFilter,
    GQLScalarFilterCore(isEq, isIn),
    defaultScalarFilter,
  )
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( QueryFormat,
    BinaryQuery,
    FromText,
    SQLEncoder(sqlEncode, ignoreInGenericInstance)
  )
import Massalia.SQLClass (SQLFilter)
import Massalia.SQLPart
  ( AQueryPart,
  )
import Data.Morpheus.Types (GQLType(description), KIND)
import Data.Morpheus.Kind (INPUT)
import Protolude

data TruckFilter
  = TruckFilter
      { id :: Maybe (GQLFilterUUID "id"),
        vehicleId :: Maybe (GQLFilterText "vehicle_id")
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON,
    SQLFilter Text, SQLFilter BinaryQuery)

instance (QueryFormat a) => SQLEncoder a TruckFilter where
  ignoreInGenericInstance = True
  sqlEncode = const ""

testInstance =
  TruckFilter
    { id = Just $ defaultScalarFilter {isEq = Just nil},
      vehicleId = Nothing
    }

instance GQLType TruckFilter where
  type KIND TruckFilter = INPUT
  description = const $ Just ("A set of filters for the Truck type" :: Text)
