{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MassaliaSchema.Industry.PlantInput
  ( PlantInput (..),
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

data PlantInput
  = PlantInput
      { id :: UUID,
        name :: Maybe Text
      }
  deriving (Eq, Show, Generic, JSON.FromJSON)

plantSchemaTriplet :: QueryFormat queryFormat => (queryFormat, [queryFormat], PlantInput -> queryFormat)
plantSchemaTriplet = undefined

data PlantListInput container
  = PlantListInput
      { plant :: container PlantInput,
        truck :: container UUID
      }
  deriving (Generic)



toInsertQuery :: QueryFormat queryFormat => PlantListInput [] -> queryFormat
toInsertQuery input = undefined

plantToQueryFormat :: QueryFormat queryFormat => PlantInput -> queryFormat
plantToQueryFormat val =
  takeParam id val §
  takeMaybeParam name val ""

plantTableColumns :: QueryFormat queryFormat => [queryFormat]
plantTableColumns = ["id", "name"]


