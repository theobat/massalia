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
import GHC.Generics (Generic)
import qualified Hasql.Encoders as Encoders
import qualified Massalia.HasqlDec as Decoders
import Data.String as StringUtils (IsString (fromString))
import Massalia.QueryFormat
  (
    SQLEncoder,
    HasqlSnippet,
    commaAssemble,
    takeMaybeParam,
    takeParam,
    (§),
  )
import Massalia.SQLWith (withXAs)
import Massalia.SQLClass (DBContext(toWithQuery), SQLName, SQLColumns, SQLValues)
import Prelude hiding (id)
import MassaliaSchema.Industry.TruckInput (TruckInput) 
import qualified Prelude (id)

data PlantInput
  = PlantInput
      { id :: UUID,
        name :: Maybe Text
      }
  deriving (
    Eq, Show, Generic, JSON.FromJSON,
    SQLName, SQLColumns, SQLValues Text
    )

data PlantListInput container
  = PlantListInput
      { plant :: container PlantInput,
        truck :: container TruckInput
      }
  deriving (Generic)

deriving instance DBContext Text (PlantListInput [])

-- toInsertQuery :: (
--     SQLEncoder Text queryFormat,
--     SQLEncoder UUID queryFormat
--   ) => PlantListInput [] -> queryFormat
-- toInsertQuery input =
--   withXAs "plant" (valuesToSelect plantSchemaTriplet) plant input

queryTest :: Text
queryTest =
  toWithQuery ()
    PlantListInput
      { plant = [PlantInput nil (Just "okokok")],
        truck = []
      }
