{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.Plant (
    Plant(..),
    plantInitSQL
) where

import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID, nil)
import Data.Text (Text)
import GHC.Generics (Generic)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import qualified Hasql.Decoders as Decoders
import Data.Morpheus.Types.Internal.AST.Selection (ValidSelection, ValidSelectionSet)
import Data.Morpheus.Types.Internal.AST.Base (Key)
import MassaliaCore (MassaliaStruct(..))

data Plant = Plant {
  id :: UUID,
  truckList :: [Truck]
} deriving (Show, Generic)

plantSelect ::
  MassaliaStruct wrapper someType Plant =>
  MassaliaStruct wrapper someType Truck =>
  (Key, ValidSelection) -> wrapper someType Plant -> wrapper someType Plant
plantSelect (fieldName, sel) = case fieldName of
  "id" -> simpleCol fieldName (\e v -> e{id=v}) id Decoders.uuid
  "truckList" -> subColList Decoders.listArray truckInitSQL (
      "EXISTS(SELECT 1 FROM truck_plant where truck_plant.plant_id=plant.id AND truck_plant.truck_id=truck.id)"
    , "plant.id") (\e v -> e{truckList=v}) sel
  _ -> Prelude.id

plantInitSQL ::
  MassaliaStruct wrapper someType Plant =>
  MassaliaStruct wrapper someType Truck =>
  ValidSelectionSet -> wrapper someType Plant
plantInitSQL = foldr plantSelect initialValue
  where initialValue = getInitialValue (dupe "plant") (Plant{ id=nil, truckList=mempty })

dupe x = (x, x)