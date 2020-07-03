{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MassaliaSchema.Industry.TruckInput
  ( TruckInput (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Data (Data)
import Data.Text (Text, pack)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified Hasql.Encoders as Encoders
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( BinaryQuery,
    FromText(fromText),
    SQLEncoder(textEncode, binaryEncode)
  )
import Massalia.SQLClass (SQLName, SQLColumns, SQLValues)
import Protolude

data TruckInput
  = TruckInput
      { id :: UUID,
        vehicleId :: Maybe Text,
        chassis :: Chassis
      }
  deriving (
    Eq, Show, Generic,
    JSON.FromJSON,
    SQLName, SQLColumns, SQLValues)

data Chassis = C8x4 | C6x4 | C4x4 | C4x2 | CUnknown
  deriving (Eq, Show, Generic, JSON.FromJSON)

instance SQLEncoder Chassis where
  textEncode = chassisToQueryFormat
  binaryEncode = fromText . chassisToQueryFormat

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

chassisToQueryFormat :: FromText queryFormat => Chassis -> queryFormat
chassisToQueryFormat ch = fromText ("row" <> (pack . show . chassisToTuple) ch <> "")
