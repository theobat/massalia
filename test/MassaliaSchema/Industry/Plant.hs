{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.Plant (
    Plant(..),
    plantInitSQL,
    plantListQuery,
    PlantListQueryFilter,
    defaultFilter
) where

import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID, nil)
import Data.Text (Text)
import GHC.Generics (Generic)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import Data.Void (Void)
import MorpheusTypes (
  Key, ValidSelection, ValidSelectionSet, Selection(Selection, selectionName, selectionContent), SelectionContent(..),
  validSelectionToSelectionSet
  )
import MassaliaSQLSelect (
    selectStructToSession,
    SelectStruct, getInitialValueSelect, scalar, RawSelectStruct(RawSelectStruct, fromPart, offsetLimit),
    initSelect, collection, testAssemblingOptions, transformWhereJoinGroup, selectStructToContentDefault    
  )
import MassaliaQueryFormat (
    QueryFormat(param, fromText), HasqlSnippet
  )
import MassaliaUtils (QueryArgsPaginated(QueryArgsPaginated, first, offset))
  
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session
import Data.Morpheus.Types (GQLRootResolver (..), GQLType, unsafeInternalContext, Context(Context, currentSelection))
import           Control.Monad.Trans            ( lift )
import PostgreSQL.Binary.Data (LocalTime)
import qualified Data.Vector as Vector

data Plant = Plant {
  id :: UUID,
  name :: Text,
  createdAt :: LocalTime,
  truckList :: [Truck]
} deriving (Show, Generic, GQLType)

type SelectStructPlant queryFormat = SelectStruct Plant queryFormat

plantSelect :: QueryFormat queryFormat => ValidSelection -> SelectStructPlant queryFormat -> SelectStructPlant queryFormat
plantSelect selection = case fieldName of
  "id" -> scalarField (\e v -> e{id=v}) Decoders.uuid
  "name" -> scalarField (\e v -> e{name=v}) Decoders.text
  "createdAt" -> scalarField (\e v -> e{createdAt=v}) Decoders.timestamp
  "truckList" -> collectionWithOptions Decoders.listArray truckSubquery (\e v -> e{truckList=v})
    where
      truckBasicSubquery = (truckInitSQL Nothing (validSelectionToSelectionSet selection))
      truckSubquery = transformWhereJoinGroup "truck_plant.plant_id=plant.id" [
          "JOIN truck_plant ON truck.id=truck_plant.truck_id"
        ] ["plant.id"] truckBasicSubquery
  _ -> Prelude.id
  where
    collectionWithOptions = collection testAssemblingOptions
    scalarField = scalar "plant" fieldName
    fieldName = selectionName selection

plantInitSQL :: QueryFormat queryFormat => PlantListQueryFilter -> ValidSelectionSet -> SelectStructPlant queryFormat
plantInitSQL filters = foldr plantSelect (initialPlantQuery filters)

initialPlantQuery :: QueryFormat queryFormat => PlantListQueryFilter -> SelectStructPlant queryFormat
initialPlantQuery filter = getInitialValueSelect initSelect{
        fromPart = "plant",
        offsetLimit = Just (offset filter, first filter)
      } defaultPlant

defaultPlant = Plant{id=nil, truckList=mempty, name =""}

plantListQuery dbConnection queryArgs = do
  Context { currentSelection = selection } <- unsafeInternalContext
  lift (exec (validSelectionToSelectionSet selection))
  where
    exec validSel = do
      res <- Session.run (statement validSel) dbConnection
      res <- Session.run (statement validSel) dbConnection
      case res of
        Left e -> (error $ show e)
        Right listRes -> pure listRes
    statement validSel = selectStructToSession $ initialSnippet validSel
    initialSnippet :: ValidSelectionSet -> SelectStructPlant HasqlSnippet
    initialSnippet = plantInitSQL defaultFilter

type PlantListQueryFilter = QueryArgsPaginated (Maybe Text)
defaultFilter = QueryArgsPaginated {
  first = 20,
  offset = 0
}

