{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}

module MassaliaSchema.Industry.PlantInput
  ( PlantInput (..),
    queryTest,
  )
where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID (nil)
import Massalia.Utils (Day)
import GHC.Generics (Generic)
import Massalia.QueryFormat
  (
    BinaryQuery,
    QueryFormat,
  )
import Massalia.SQLClass (
    DBContext(toWithQuery), SQLName, SQLColumns, SQLValues,
    defaultWithQueryOption, DBContextSubquery(withSubqueryFromCollection), 
    insertDBContextSubquery
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
    SQLName, SQLColumns, SQLValues
    )
instance DBContextSubquery c PlantInput where
  withSubqueryFromCollection a b = insertDBContextSubquery a b

data PlantListInput container
  = PlantListInput
      { plant :: container PlantInput,
        truck :: container TruckInput
      }
  deriving (Generic)

deriving instance DBContext (PlantListInput [])
-- deriving instance DBContext BinaryQuery (PlantListInput [])

queryTest :: (QueryFormat qf, DBContext (PlantListInput [])) => qf
queryTest =
  toWithQuery (Just defaultWithQueryOption)
    PlantListInput
      { plant = [PlantInput nil (Just "okokok") (JSON.decode "\"1991-08-22\"")],
        truck = []
      }
