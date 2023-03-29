{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MassaliaSchema.TestAPI where

import GHC.Generics (Generic)
import Massalia.HasqlExec (Pool)
import Massalia.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    RootResolver (..),
    GQLType,
    Undefined (..),
    defaultRootResolver,
    Resolver,
    QUERY
  )
import MassaliaSchema.Industry.Plant (Plant, plantListQuery, plantListQueryGen)
import MassaliaSchema.Industry.PlantFilter (PlantFilter)
import Massalia.UtilsGQL (Paginated)
import Data.Vector (Vector)

api :: Pool -> GQLRequest -> IO GQLResponse
api dbConnectionPool = interpreter $ rootResolver (Just dbConnectionPool)

apiWithoutDB :: GQLRequest -> IO GQLResponse
apiWithoutDB = interpreter $ rootResolver Nothing

rootResolver :: _ => Maybe Pool -> RootResolver m () Query Undefined Undefined
rootResolver dbConnectionPool =
  defaultRootResolver
    { queryResolver = rootQuery dbConnectionPool
    }

-- data Mutation m
--   = Mutation
--       { plantListInsert :: PlantInputDBContext -> m [Plant]
--       }
--   deriving (Generic, GQLType)

data Query m
  = Query
      { plantListPaginated :: Paginated PlantFilter -> m (Vector Plant),
        plantListQueryGenerator :: Paginated PlantFilter -> m (Vector Plant)
      }
  deriving (Generic, GQLType)

rootQuery :: _ => (Maybe Pool) -> Query m
rootQuery dbConnectionPool =
  Query
    { plantListPaginated = plantListQuery dbConnectionPool,
      plantListQueryGenerator = plantListQueryGen
    }
