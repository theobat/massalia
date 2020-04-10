{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.TruckInput (
    TruckInput(..)
) where
import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import qualified Hasql.Decoders as Decoders
import qualified Data.Aeson as JSON
import MassaliaQueryFormat (
    QueryFormat(param, fromText), HasqlSnippet
  )

data TruckInput = TruckInput {
  id :: UUID
  , vehicleId :: Text
} deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)


truckInputToQueryFormat :: QueryFormat queryFormat => TruckInput -> queryFormat
truckInputToQueryFormat truckInput = convert $ id truckInput <> convert vehicleId truckInput

convert :: QueryFormat queryFormat => UUID -> queryFormat