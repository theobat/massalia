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
import Data.Morpheus.Types.Internal.AST.Selection (ValidSelection, ValidSelectionSet, Selection(Selection, selectionContent), SelectionContent(..))
import Data.Morpheus.Types.Internal.AST.Base (Key)
import MassaliaSQLSelect (
    SelectStruct, getInitialValueSelect, scalar, RawSelectStruct(RawSelectStruct, fromPart),
    defaultSelect, collection, testAssemblingOptions, transformWhereJoinGroup
  )
import MassaliaQueryFormat (
    QueryFormat(param, fromText)
  )
import qualified Hasql.Decoders as Decoders

data Plant = Plant {
  id :: UUID,
  truckList :: [Truck]
} deriving (Show, Generic)

-- plantSelect ::
--   MassaliaStruct wrapper someType Plant =>
--   MassaliaStruct wrapper someType Truck =>
--   (Key, ValidSelection) -> wrapper someType Plant -> wrapper someType Plant
-- plantSelect (fieldName, sel) = case fieldName of
--   "id" -> simpleCol fieldName (\e v -> e{id=v}) id Decoders.uuid
--   "truckList" -> subColList Decoders.listArray truckInitSQL (
--       "EXISTS(SELECT 1 FROM truck_plant where truck_plant.plant_id=plant.id AND truck_plant.truck_id=truck.id)"
--     , "plant.id") (\e v -> e{truckList=v}) sel
--   _ -> Prelude.id


plantSelect :: QueryFormat queryFormat => (Key, ValidSelection) -> SelectStruct Plant queryFormat -> SelectStruct Plant queryFormat
plantSelect (fieldName, sel) = case fieldName of
  "id" -> scalar fieldName (\e v -> e{id=v}) Decoders.uuid
  "truckList" -> collection testAssemblingOptions Decoders.listArray truckSubquery (\e v -> e{truckList=v})
    where
      truckBasicSubquery = (truckInitSQL () (validSelectionToSelectionSet sel))
      truckSubquery = transformWhereJoinGroup "truck_plant.plant_id=plant.id" [
          "JOIN truck_plant ON truck.id=truck_plant.truck_id"
        ] ["plant.id"] truckBasicSubquery
  _ -> Prelude.id

plantInitSQL :: QueryFormat queryFormat => () -> ValidSelectionSet -> SelectStruct Plant queryFormat
plantInitSQL filters = foldr plantSelect (initialPlantQuery filters)

initialPlantQuery :: QueryFormat queryFormat => () -> SelectStruct Plant queryFormat
initialPlantQuery _ = getInitialValueSelect defaultSelect{
        fromPart = "plant"
      } defaultPlant

defaultPlant = Plant{}

validSelectionToSelectionSet (Selection{ selectionContent = selection }) = case selection of
  SelectionField -> error "graphql validation should prevent this, it should not exist"
  (SelectionSet deeperSel) -> deeperSel
