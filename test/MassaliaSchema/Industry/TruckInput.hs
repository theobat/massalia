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
import Data.Text (Text, pack)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as Decoders
import MassaliaQueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
    (§),
    takeParam,
    takeMaybeParam
  )
import Prelude hiding (id)
import qualified Prelude (id)
import qualified Hasql.Encoders as Encoders

data TruckInput
  = TruckInput
      { id :: UUID,
        vehicleId :: Maybe Text,
        chassis :: Chassis
      }
  deriving (Show, Generic, JSON.FromJSON)

data Chassis = C8x4 | C6x4 | C4x4 | C4x2 | CUnknown deriving (Show, Generic, JSON.FromJSON)

chassisFromTuple :: (Int, Int) -> Chassis
chassisFromTuple value = case value of
  (8, 4) -> C8x4
  (6, 4) -> C6x4
  (4, 4) -> C4x4
  (4, 2) -> C4x2
  _ -> CUnknown

chassisToTuple :: Chassis -> (Int, Int)
chassisToTuple value = case value of
  C8x4 -> (8, 4)
  C6x4 -> (6, 4)
  C4x4 -> (4, 4)
  C4x2 -> (4, 2)
  CUnknown -> (-1, -1)

data DefaultBehavior queryFormat = Mandatory | Default queryFormat
data TableColumnPolicy = FirstElement | RemoveTheAllEmpty | AlwaysFull

chassisToQueryFormat :: QueryFormat queryFormat => Chassis -> queryFormat
chassisToQueryFormat ch = fromText ("row(" <> (pack . show . chassisToTuple) ch <> ")")

toQueryFormat :: QueryFormat queryFormat => TruckInput -> queryFormat
toQueryFormat val =
  takeParam id val §
  takeMaybeParam vehicleId val "" §
  chassisToQueryFormat (chassis val)

tableColumns :: QueryFormat queryFormat => [queryFormat]
tableColumns = ["id", "vehicle_id", "chassis"]


