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
  deriving (Show, Generic)

data Chassis = C8x4 | C6x4 | C4x4 | C4x2 | CUnknown deriving (Show, Generic)

chassisFromTuple :: (Int, Int) -> Chassis
chassisFromTuple _ = CUnknown
chassisToTuple :: Chassis -> (Int, Int)
chassisToTuple C8x4 = (8, 4)
chassisToTuple C6x4 = (6, 4)
chassisToTuple C4x4 = (4, 4)
chassisToTuple C4x2 = (4, 2)
chassisToTuple CUnknown = (-1, -1)

data DefaultBehavior queryFormat = Mandatory | Default queryFormat
data TableColumnPolicy = FirstElement | RemoveTheAllEmpty | AlwaysFull

chassisToQueryFormat :: QueryFormat queryFormat => Chassis -> queryFormat
chassisToQueryFormat ch = fromText ("row(" <> (pack . show . chassisToTuple) ch <> ")")

toQueryFormat :: QueryFormat queryFormat => TruckInput -> queryFormat
toQueryFormat val =
  takeParam id val §
  takeMaybeParam vehicleId val "" §
  chassisToQueryFormat (chassis val)
  where
    (§) a b = a <> "," <> b 
    takeParam a b = param $ a b
    takeMaybeParam a b c = case (a b) of
      Nothing -> c
      Just d -> param d

tableColumns :: QueryFormat queryFormat => [queryFormat]
tableColumns = ["id", "vehicle_id", "chassis"]
