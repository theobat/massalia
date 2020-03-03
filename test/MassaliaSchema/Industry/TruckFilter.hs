{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module MassaliaSchema.Industry.TruckFilter (
    TruckFilter(..)
) where
import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import MassaliaCore (MassaliaStruct(..))
import qualified Hasql.Decoders as Decoders
import qualified Data.Aeson as JSON
import MassaliaFilter (GQLFilterUUID, GQLFilterText, defaultScalarFilter, GQLScalarFilter(isIn), filterFieldToQueryPart)

data TruckFilter = TruckFilter {
  id :: GQLFilterUUID "id",
  vehicleId :: GQLFilterText "vehicle_id"
} deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)


