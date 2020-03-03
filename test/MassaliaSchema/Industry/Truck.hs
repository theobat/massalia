{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.Truck (
    Truck(..),
    truckInitSQL
) where
import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID, nil)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import MassaliaSQLSelect (SelectStruct, getInitialValueSelect, scalar, RawSelectStruct(RawSelectStruct, fromPart), defaultSelect)
import MassaliaQueryFormat (
    QueryFormat(param, fromText)
  )
import qualified Hasql.Decoders as Decoders
import Data.Morpheus.Types.Internal.AST.Selection (ValidSelection, ValidSelectionSet)
import Data.Morpheus.Types.Internal.AST.Base (Key)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import qualified MassaliaSchema.Industry.TruckFilter as TruckFilter

data Truck = Truck {
  id :: UUID
  , vehicleId :: Text
} deriving (Show, Generic)


truckSelect :: QueryFormat content => (Key, ValidSelection) -> SelectStruct Truck content -> SelectStruct Truck content
truckSelect (fieldName, _) = case fieldName of
  "id" -> scalar fieldName (\e v -> e{id=v}) Decoders.uuid
  -- "vehicleId" -> simpleCol fieldName (\e v -> e{vehicleId=v}) vehicleId Decoders.text
  _ -> Prelude.id

truckInitSQL :: QueryFormat content => () -> ValidSelectionSet -> SelectStruct Truck content
truckInitSQL filters = foldr truckSelect (initialTruckQuery filters)

initialTruckQuery :: QueryFormat content => () -> SelectStruct Truck content
initialTruckQuery _ = getInitialValueSelect defaultSelect{
        fromPart = "truck"
      } defaultTruck

defaultTruck = Truck nil "test_id"


data TruckListFilter = TruckListFilter {
  truck :: Maybe TruckFilter
}