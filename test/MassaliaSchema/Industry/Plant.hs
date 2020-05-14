{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaSchema.Industry.Plant
  ( Plant (..),
    plantListQuery,
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
import Massalia.SelectionTree (MassaliaTree (getName, foldrChildren), fromMorpheusContext)
import Massalia.MorpheusTypes
  (
    GQLRootResolver (..),
    GQLType,
  )
import Massalia.QueryFormat
  ( HasqlSnippet,
    SQLEncoder,
    DefaultParamEncoder,
    FromText(fromText),
    QueryFormat,
    SQLDecoder(sqlDecode)
  )
import Massalia.SQLSelect
  ( RawSelectStruct (RawSelectStruct, fromPart, whereConditions, offsetLimit),
    SelectStruct(SelectStruct),
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
import MassaliaSchema.Industry.PlantInput (queryTest)
import MassaliaSchema.Industry.Truck (Truck)
import Protolude hiding (first)
import Text.Pretty.Simple (pPrint)
import Massalia.HasqlExec (Pool, use)
import MassaliaSchema.Industry.PlantFilter (PlantFilter, plantFilterTest)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Massalia.SQLClass (SQLFilter(toQueryFormatFilter))
import Data.Morpheus.Types (unsafeInternalContext)
import Massalia.Utils (LocalTime, Day)
import Massalia.UtilsGQL (Paginated, defaultPaginated)
import Massalia.SQLRawSelect (addSelectColumns)
import qualified Massalia.UtilsGQL as Paginated(first, offset, filtered)
import Massalia.SQLClass (
    SQLColumn(toColumnListAndDecoder),
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    SQLFilter(toQueryFormatFilter),
    SQLColumnConfig(..)
  )

data Plant
  = Plant
      { id :: UUID,
        name :: Text,
        createdAt :: LocalTime,
        checkDate :: Day,
        truckList :: [Truck]
      }
  deriving (Show, Generic, GQLType,
    SQLColumn Text PlantFilter, SQLColumn HasqlSnippet PlantFilter)

instance (
    QueryFormat queryFormat,
    SQLFilter queryFormat PlantFilter
  ) => SQLSelect queryFormat PlantFilter Plant where
  toSelectQuery opt selection filter = SelectStruct queryWithColumnList decoder
    where
      queryWithColumnList = addSelectColumns (pure <$> colList) [] rawQuery
      (colList, decoder) = toColumnListAndDecoder (SQLColumnConfig instanceName) selection realFilter
      realFilter = Paginated.filtered filter
      rawQuery = initialPlantQuery (fromText $ instanceName) filter
      instanceName = "plant"

instance (QueryFormat queryFormat) => SQLDecoder queryFormat PlantFilter [Truck] where
  sqlDecode = undefined

        -- transformWhereJoinGroup
        --   ("truck_plant.plant_id=" <> pure tableNameQueryFormat <> ".id")
        --   [ "JOIN truck_plant ON truck.id=truck_plant.truck_id"
        --   ]
        --   [pure tableNameQueryFormat <> ".id"]
        --   truckBasicSubquery

initialPlantQuery :: (
    SQLFilter queryFormat PlantFilter,
    Monoid queryFormat, IsString queryFormat
  ) =>
  queryFormat -> Paginated PlantFilter -> RawSelectStruct queryFormat
initialPlantQuery name filter = initSelect
      { fromPart = pure name,
        whereConditions = pure <$> toQueryFormatFilter Nothing <$> (Paginated.filtered filter),
        offsetLimit = Just (
          fromMaybe 0 $ Paginated.offset filter, fromMaybe 10000 $ Paginated.first filter)
      }

instance SQLDefault Plant where
  getDefault = defaultPlant
defaultPlant = Plant {id = nil, truckList = mempty, name = ""}

plantListQuery pool queryArgs = do
  massaliaTree <- (pure . fromMorpheusContext) =<< unsafeInternalContext
  lift (exec massaliaTree)
  where
    exec validSel = do
      let (snippet, result) = selectStructToSnippetAndResult $ initialSnippet validSel
      let fullSnippet = queryTest <> " " <> snippet
      let session = dynamicallyParameterizedStatement fullSnippet result
      res <- use pool session
      case res of
        Left e -> (panic $ show e)
        Right listRes -> pure listRes
    statement validSel = selectStructToSession $ initialSnippet validSel
    initialSnippet selSet = toSelectQuery Nothing selSet arg
    arg = defaultPaginated{Paginated.filtered = Just plantFilterTest}

