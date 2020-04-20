{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import Massalia.QueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
    commaAssemble,
    takeMaybeParam,
    takeParam,
    (§),
  )
import Massalia.SQLInsert (InsertSchema (InsertSchema), valuesToSelect)
import Massalia.SQLWith (withXAs)
import Prelude hiding (id)
import qualified Prelude (id)

data PlantInput
  = PlantInput
      { id :: UUID,
        name :: Maybe Text
      }
  deriving (Eq, Show, Generic, JSON.FromJSON)

plantSchemaTriplet :: QueryFormat queryFormat => InsertSchema queryFormat PlantInput
plantSchemaTriplet = InsertSchema $ ("plant", ["id", "name"], plantToQueryFormat)
  where
    plantToQueryFormat val =
      takeParam id val
        § takeMaybeParam name val "''"

csc :: QueryFormat queryFormat => queryFormat
csc = commaAssemble columnList
  where
    InsertSchema (_, columnList, _) = plantSchemaTriplet

-- plantCollectionToQueryFormat = valuesFormatter Nothing (Just csc)

data PlantListInput container
  = PlantListInput
      { plant :: container PlantInput,
        truck :: container UUID
      }
  deriving (Generic)

toInsertQuery :: QueryFormat queryFormat => PlantListInput [] -> queryFormat
toInsertQuery input =
  withXAs "plant" (valuesToSelect plantSchemaTriplet) plant input

queryTest :: QueryFormat queryFormat => queryFormat
queryTest =
  toInsertQuery
    PlantListInput
      { plant = [PlantInput nil (Just "okokok")],
        truck = []
      }
