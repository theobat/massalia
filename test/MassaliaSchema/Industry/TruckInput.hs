{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

module MassaliaSchema.Industry.TruckInput
  ( TruckInput (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Data (Data)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as Decoders
import MassaliaQueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
  )
import Prelude hiding (id)
import qualified Prelude (id)

data TruckInput
  = TruckInput
      { id :: UUID,
        vehicleId :: Text
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

-- truckInputToQueryFormat :: QueryFormat queryFormat => TruckInput -> queryFormat
-- truckInputToQueryFormat truckInput = convert $ id truckInput <> convert $ vehicleId truckInput

-- convert :: QueryFormat queryFormat => forall a. a -> queryFormat
-- convert = undefined
