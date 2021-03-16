{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE PartialTypeSignatures #-}
module MassaliaSchema.Industry.Plant
  ( Plant (..),
    plantListQuery,
    plantListQueryGen
  )
where
import Protolude
import Data.UUID (UUID, nil)
import Massalia.HasqlExec (dynamicallyParameterizedStatement, use)
import Massalia.SelectionTree (fromMorpheusContext)
import Massalia.MorpheusTypes
  (
    GQLType,
  )
import Massalia.QueryFormat
  ( BinaryQuery,
    QueryFormat,
    SQLDecoder(sqlExpr),
    SQLEncoder,
    joinEq,
    simpleEq,
    (°),DedupeBinaryQuery (binaryQueryResult)
  )
import Massalia.SQLSelectStruct (
  SelectStruct(..),
  queryAndDecoderToQueryFormatAndResult, queryAndDecoderToQueryFormat
  )
import MassaliaSchema.Industry.PlantInput (queryTest)
import MassaliaSchema.Industry.Truck (Truck)
import MassaliaSchema.Industry.PlantFilter (PlantFilter, plantFilterTest)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Data.Morpheus.Types (unsafeInternalContext)
import Massalia.Utils (LocalTime, Day, uuidV4)
import Massalia.UtilsGQL (Paginated, defaultPaginated)
import qualified Massalia.UtilsGQL as Paginated
import Massalia.SQLClass (
    SQLRecord,
    SQLSelect(toSelectQuery),
    SQLDefault(getDefault),
    basicQueryAndDecoder,
    basicEntityQuery
  )
import qualified Massalia.Default as Default

data Plant
  = Plant
      { id :: UUID,
        name :: Text,
        createdAt :: LocalTime,
        checkDate :: Day,
        description :: Maybe Text,
        truckList :: [Truck]
      }
  deriving (Show, Generic, GQLType)
deriving instance SQLRecord (Paginated PlantFilter) Plant

instance SQLSelect (Paginated PlantFilter) Plant where
  toSelectQuery = basicQueryAndDecoder (basicEntityQuery "plant" Just)

instance SQLDefault Plant where
  getDefault = defaultPlant
defaultPlant :: Plant
defaultPlant = Plant {
    id = nil,
    truckList = mempty,
    name = "",
    description= Nothing,
    checkDate=Default.date,
    createdAt=Default.timestamptz
  }

plantListQuery :: _ => _ -> _ -> _ _ _ IO [Plant]
plantListQuery maybePool queryArgs = do
  massaliaTree <- fromMorpheusContext <$> unsafeInternalContext
  case maybePool of
    Nothing -> pure []
    Just pool -> lift (exec pool massaliaTree)
  where
    exec :: _ => _ -> _ -> _ [Plant]
    exec pool validSel = do
      let (snippet, result) = queryAndDecoderToQueryFormatAndResult $ toSelectQuery validSel arg
      let fullSnippet = binaryQueryResult snippet
      let session = dynamicallyParameterizedStatement fullSnippet result True
      res <- use pool session
      case res of
        Left e -> panic $ show e
        Right listRes -> pure listRes
    -- statement validSel = queryAndDecoderToSession $ initialSnippet validSel
    arg = defaultPaginated{Paginated.globalFilter = pure plantFilterTest}

plantListQueryGen :: _ => Paginated PlantFilter -> _ _ _ IO [Plant]
plantListQueryGen queryArgs = do
  massaliaTree <- fromMorpheusContext <$> unsafeInternalContext
  let (snippet, _) = queryAndDecoderToQueryFormatAndResult $ toSelectQuery @_ @Plant massaliaTree queryArgs
  pure [defaultPlant{name=snippet}]