{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

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
import MassaliaSQLSelect (SelectStruct, getInitialValueSelect, scalar, RawSelectStruct(RawSelectStruct, fromPart, whereConditions), initSelect)
import MassaliaQueryFormat (
    QueryFormat(param, fromText)
  )
import qualified Hasql.Decoders as Decoders
import MorpheusTypes (ValidSelection, ValidSelectionSet, Selection(selectionName), Key)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import qualified MassaliaSchema.Industry.TruckFilter as TruckFilter

import Data.Morpheus.Types (GQLRootResolver (..), GQLType)

data Truck = Truck {
  id :: UUID
  , vehicleId :: Text
} deriving (Show, Generic, GQLType)

truckSelect :: QueryFormat content => ValidSelection -> SelectStruct Truck content -> SelectStruct Truck content
truckSelect selection = case fieldName of
  "id" -> scalarField (\e v -> e{id=v}) Decoders.uuid
  -- "vehicleId" -> simpleCol fieldName (\e v -> e{vehicleId=v}) vehicleId Decoders.text
  _ -> Prelude.id
  where
    scalarField = scalar "truck" fieldName
    fieldName = selectionName selection

truckInitSQL :: QueryFormat content => Maybe TruckListFilter -> ValidSelectionSet -> SelectStruct Truck content
truckInitSQL filters = foldr truckSelect (initialTruckQuery filters)

initialTruckQuery :: QueryFormat content => Maybe TruckListFilter -> SelectStruct Truck content
initialTruckQuery filterw = getInitialValueSelect initSelect{
        fromPart = "truck",
        whereConditions = toQueryPart filterw
      } defaultTruck

defaultTruck = Truck nil ""

data TruckListFilter = TruckListFilter {
  truck :: Maybe TruckFilter
}

toQueryPart Nothing = mempty
toQueryPart (Just listFilter) =
    TruckFilter.toQueryPart (truck listFilter)
