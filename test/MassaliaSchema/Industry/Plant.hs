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
import MorpheusTypes (ValidSelection, ValidSelectionSet, Selection(Selection, selectionName, selectionContent), SelectionContent(..))
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

type SelectStructPlant queryFormat = SelectStruct Plant queryFormat

plantSelect :: QueryFormat queryFormat => ValidSelection -> SelectStructPlant queryFormat -> SelectStructPlant queryFormat
plantSelect selection = case fieldName of
  "id" -> scalar fieldName (\e v -> e{id=v}) Decoders.uuid
  "truckList" -> collection testAssemblingOptions Decoders.listArray truckSubquery (\e v -> e{truckList=v})
    where
      truckBasicSubquery = (truckInitSQL Nothing (validSelectionToSelectionSet selection))
      truckSubquery = transformWhereJoinGroup "truck_plant.plant_id=plant.id" [
          "JOIN truck_plant ON truck.id=truck_plant.truck_id"
        ] ["plant.id"] truckBasicSubquery
  _ -> Prelude.id
  where
    fieldName = selectionName selection


plantInitSQL :: QueryFormat queryFormat => Maybe () -> ValidSelectionSet -> SelectStructPlant queryFormat
plantInitSQL filters = foldr plantSelect (initialPlantQuery filters)

initialPlantQuery :: QueryFormat queryFormat => Maybe () -> SelectStructPlant queryFormat
initialPlantQuery _ = getInitialValueSelect defaultSelect{
        fromPart = "plant"
      } defaultPlant

defaultPlant = Plant{}

validSelectionToSelectionSet (Selection{ selectionContent = selection }) = case selection of
  SelectionField -> error "graphql validation should prevent this, it should not exist"
  (SelectionSet deeperSel) -> deeperSel
