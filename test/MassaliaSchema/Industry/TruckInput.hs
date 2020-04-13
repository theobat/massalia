{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import qualified Hasql.Encoders as Encoders

data TruckInput
  = TruckInput
      { id :: UUID,
        vehicleId :: Maybe Text,
        chassis :: (Int, Int)
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

data DefaultBehavior queryFormat = Mandatory | Default queryFormat
data TableColumnPolicy = FirstElement | RemoveTheAllEmpty | AlwaysFull

-- toQueryFormat :: QueryFormat queryFormat => TruckInput -> queryFormat
-- toQueryFormat value = takeParam id value ++ (takeParam chassis value) -- <> param $ chassis value
--   where
--     (++) a b = a <> "," <> b 
--     takeParam a b = param a b

tableColumns :: QueryFormat queryFormat => () -> [(queryFormat, DefaultBehavior queryFormat)]
tableColumns = const [
    ("id", Mandatory),
    ("vehicle_id", Default ""),
    ("chassis", Mandatory)
  ]

-- okok = Encoders.nonNullable Encoders.

-- parametrizer :: QueryFormat queryFormat => TruckInput -> queryFormat
-- parametrizer input = 

-- data InsertOptions = InsertOptions {
--   tableColumnsPolicy :: TableColumnPolicy
-- }

truckInputToInsertHeader ::
  (QueryFormat queryFormat, Foldable collection) =>
  (a -> queryFormat) ->
  collection a ->
  queryFormat
truckInputToInsertHeader = undefined

-- truckInputToQueryFormat :: QueryFormat queryFormat => TruckInput -> queryFormat
-- truckInputToQueryFormat truckInput = convert $ id truckInput <> convert $ vehicleId truckInput

-- hasqlConverter :: TruckInput -> Snippet

-- convert :: QueryFormat queryFormat => forall a. a -> queryFormat
-- convert = undefined

