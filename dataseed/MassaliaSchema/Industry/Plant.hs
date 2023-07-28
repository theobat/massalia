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
import Massalia.SelectionTree (fromMorpheusContextM)
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
  queryAndDecoderToQueryFormatAndResultVect, queryAndDecoderToQueryFormat
  )
import MassaliaSchema.Industry.PlantInput (queryTest)
import MassaliaSchema.Industry.Truck (Truck)
import MassaliaSchema.Industry.PlantFilter (PlantFilter)
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
import Data.Vector (Vector)

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

plantListQuery :: _ => Maybe _ -> Paginated PlantFilter -> m (Vector Plant)
plantListQuery maybePool queryArgs = do
  massaliaTree <- fromMorpheusContextM
  case maybePool of
    Nothing -> pure mempty
    Just pool -> liftIO (exec pool massaliaTree)
  where
    exec :: _ => _ -> _ -> IO (Vector Plant)
    exec pool validSel = do
      let (snippet, result) = queryAndDecoderToQueryFormatAndResultVect $ toSelectQuery validSel queryArgs
      let fullSnippet = binaryQueryResult snippet
      let session = dynamicallyParameterizedStatement fullSnippet result True
      res <- use pool session
      case res of
        Left e -> panic $ show e
        Right listRes -> pure listRes
    -- statement validSel = queryAndDecoderToSession $ initialSnippet validSel
    -- arg = defaultPaginated{Paginated.globalFilter = pure plantFilterTest}

plantListQueryGen :: _ => Paginated PlantFilter -> m (Vector Plant)
plantListQueryGen queryArgs = do
  massaliaTree <- fromMorpheusContextM
  let (snippet, _) = queryAndDecoderToQueryFormatAndResultVect $ toSelectQuery @_ @Plant massaliaTree queryArgs
  pure $ pure defaultPlant{name=snippet} -- a Hack for only generating the query