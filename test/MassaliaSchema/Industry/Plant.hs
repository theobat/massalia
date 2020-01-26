{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.Plant (
    Plant(..)
) where

import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import SQL (Aggregable(simpleCol))
import MassaliaSchema.Industry.Truck (Truck)
import qualified Hasql.Decoders as Decoders

-- import Truck (Truck)

data Plant = Plant {
  id :: UUID,
  truckList :: [Truck]
} deriving (Show, Generic)

plantMassalia :: Aggregable wrapper someType Plant => Text -> wrapper someType Plant -> wrapper someType Plant
plantMassalia "id" = simpleCol "id" (\e v -> e{id=v}) id Decoders.uuid
plantMassalia _ = Prelude.id
