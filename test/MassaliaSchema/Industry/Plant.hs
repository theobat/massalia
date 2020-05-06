{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}

module MassaliaSchema.Industry.Plant
  ( Plant (..),
    plantInitSQL,
    plantListQuery,
    PlantListQueryFilter,
    defaultFilter,
  )
where

import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.UUID (UUID, nil)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import qualified Hasql.Connection as Connection
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Massalia.HasqlDec as Decoders
import Massalia.HasqlExec (dynamicallyParameterizedStatement)
import Massalia.MorpheusTypes
  ( Key,
    Selection (Selection, selectionContent, selectionName),
    SelectionContent (..),
    ValidSelection,
    ValidSelectionSet,
    validSelectionToSelectionSet,
  )
import Massalia.MorpheusTypes
  ( Context (Context, currentSelection),
    GQLRootResolver (..),
    GQLType,
    unsafeInternalContext,
  )
import Massalia.QueryFormat
  ( HasqlSnippet,
    SQLEncoder,
    DefaultParamEncoder
  )
import Massalia.SQLSelect
  ( RawSelectStruct (RawSelectStruct, fromPart, offsetLimit),
    SelectStruct,
    collection,
    getInitialValueSelect,
    initSelect,
    scalar,
    selectStructToContentDefault,
    selectStructToSession,
    selectStructToSnippetAndResult,
    testAssemblingOptions,
    transformWhereJoinGroup,
  )
import Massalia.UtilsGQL (QueryArgsPaginated (QueryArgsPaginated, first, offset))
import MassaliaSchema.Industry.PlantInput (queryTest)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import PostgreSQL.Binary.Data (LocalTime)
import Protolude hiding (first)
import Text.Pretty.Simple (pPrint)

data Plant
  = Plant
      { id :: UUID,
        name :: Text,
        createdAt :: LocalTime,
        truckList :: [Truck]
      }
  deriving (Show, Generic, GQLType)

type SelectStructPlant queryFormat = SelectStruct Plant queryFormat

plantSelect ::  (
    SQLEncoder Int64 queryFormat
  ) =>
  ValidSelection -> SelectStructPlant queryFormat -> SelectStructPlant queryFormat
plantSelect selection = case fieldName of
  "id" -> scalarField (\e v -> e {id = v}) Decoders.uuid
  "name" -> scalarField (\e v -> e {name = v}) Decoders.text
  "createdAt" -> scalarField (\e v -> e {createdAt = v}) Decoders.timestamp
  "truckList" -> collection testAssemblingOptions Decoders.listArray truckSubquery (\e v -> e {truckList = v})
    where
      truckBasicSubquery = (truckInitSQL Nothing (validSelectionToSelectionSet selection))
      truckSubquery =
        transformWhereJoinGroup
          "truck_plant.plant_id=plant.id"
          [ "JOIN truck_plant ON truck.id=truck_plant.truck_id"
          ]
          ["plant.id"]
          truckBasicSubquery
  _ -> identity
  where
    scalarField = scalar "plant" fieldName
    fieldName = selectionName selection

plantInitSQL ::  (
    SQLEncoder Int64 queryFormat
  ) =>
  PlantListQueryFilter -> ValidSelectionSet -> SelectStructPlant queryFormat
plantInitSQL filters = foldr plantSelect (initialPlantQuery filters)

initialPlantQuery :: (Monoid queryFormat, IsString queryFormat) =>
  PlantListQueryFilter -> SelectStructPlant queryFormat
initialPlantQuery filter =
  getInitialValueSelect
    initSelect
      { fromPart = "plant",
        offsetLimit = Just (offset filter, first filter)
      }
    defaultPlant

defaultPlant = Plant {id = nil, truckList = mempty, name = ""}

plantListQuery dbConnection queryArgs = do
  Context {currentSelection = selection} <- unsafeInternalContext
  lift (exec (validSelectionToSelectionSet selection))
  where
    exec validSel = do
      -- res <- Session.run (statement validSel) dbConnection
      let (snippet, result) = selectStructToSnippetAndResult $ initialSnippet validSel
      let fullSnippet = "SELECT 1" -- (queryTest) <> " " <> snippet
      res <- Session.run (dynamicallyParameterizedStatement fullSnippet result) dbConnection
      -- case res2 of
      --   Left e -> (error $ show e)
      --   Right listRes -> pPrint listRes
      case res of
        Left e -> (panic $ show e)
        Right listRes -> pure listRes
    statement validSel = selectStructToSession $ initialSnippet validSel
    initialSnippet :: ValidSelectionSet -> SelectStructPlant HasqlSnippet
    initialSnippet = plantInitSQL defaultFilter

type PlantListQueryFilter = QueryArgsPaginated (Maybe Text)

defaultFilter =
  QueryArgsPaginated
    { first = 20,
      offset = 0
    }
