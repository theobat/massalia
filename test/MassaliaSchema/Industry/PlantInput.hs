{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaSchema.Industry.PlantInput
  ( PlantInput (..),
    queryTest,
  )
where

import qualified Data.Aeson as JSON
import Data.Data (Data)
import Data.Text (Text, pack)
import Data.UUID (UUID)
import Data.UUID (nil)
import Massalia.Utils (Day)
import GHC.Generics (Generic)
import Massalia.QueryFormat
  (
    BinaryQuery,
  )
import Massalia.SQLClass (
    DBContext(toWithQuery), SQLName, SQLColumns, SQLValues,
    WithQueryOption(PureSelect)
  )
import MassaliaSchema.Industry.TruckInput (TruckInput)

data PlantInput
  = PlantInput
      { id :: UUID,
        name :: Maybe Text,
        checkDate :: Maybe Day
      }
  deriving (
    Eq, Show, Generic, JSON.FromJSON,
    SQLName, SQLColumns,
    SQLValues Text, SQLValues BinaryQuery
    )

data PlantListInput container
  = PlantListInput
      { plant :: container PlantInput,
        truck :: container TruckInput
      }
  deriving (Generic)

deriving instance DBContext Text (PlantListInput [])
deriving instance DBContext BinaryQuery (PlantListInput [])

queryTest :: (DBContext queryFormat (PlantListInput [])) => queryFormat
queryTest =
  toWithQuery (Just PureSelect)
    PlantListInput
      { plant = [PlantInput nil (Just "okokok") (JSON.decode "\"1991-08-22\"")],
        truck = []
      }
