{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.Truck (
    Truck(..)
) where
import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import SQL (Aggregable(simpleCol))
import qualified Hasql.Decoders as Decoders

data Truck = Truck {
  id :: UUID
  , vehicleId :: Text
} deriving (Show, Generic)

truckMassalia :: Aggregable wrapper someType Truck => Text -> wrapper someType Truck -> wrapper someType Truck
truckMassalia "id" = simpleCol "id" (\e v -> e{id=v}) id Decoders.uuid
truckMassalia "vehicleId" = simpleCol "vehicle_id" (\e v -> e{vehicleId=v}) vehicleId Decoders.text
truckMassalia _ = Prelude.id

